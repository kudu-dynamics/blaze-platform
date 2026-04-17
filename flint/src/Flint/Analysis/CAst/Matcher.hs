module Flint.Analysis.CAst.Matcher
  ( runCheck
  , runCheckCapped
  , runCheckOnStmts
  , matchStmtPat
  , matchExprPat
  , CheckResult(..)
  , CMatcherState(..)
  , maxFindingsPerCheck
  ) where

import Flint.Prelude hiding (group, get, put, gets, State, StateT, runStateT, check)

import Flint.Types.Analysis.CAst.Matcher
import Flint.Types.Analysis.CAst.Finding

import Blaze.Types.CAst
  ( CStmt(..), CExpr(..), CForInit(..), CCase(..)
  , CAnn, AddrRange
  , exprAnn, stmtAnn
  , renderStmts, renderExpr
  )

import Control.Monad.Logic (LogicT, observeMany)
import Control.Monad.Trans.State.Strict (StateT, get, put, modify', runStateT, gets)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Text.Regex.TDFA ((=~))


-- | Maximum number of unique findings reported per check per scan unit.
-- Repetitive patterns beyond this cap are reported as truncated so callers
-- can surface a warning, rather than silently dropping results as the old
-- @observeMany 50@ did.
maxFindingsPerCheck :: Int
maxFindingsPerCheck = 20

-- | Raw bound on the underlying LogicT stream. Sits well above
-- 'maxFindingsPerCheck' because 'CStar' (and other non-deterministic
-- combinators) can produce many duplicate match states for a single
-- real finding; we need enough headroom for dedupe to extract up to
-- 'maxFindingsPerCheck' unique results on realistic inputs without
-- letting a pathological pattern run unbounded.
rawObservationLimit :: Int
rawObservationLimit = 1000


-- | Matcher state.
--
-- 'matchedStmts' records the CStmt nodes that actually satisfied a pattern
-- at this nesting level, in source order. This is what gets rendered as
-- 'cSource' on a finding — not the prefix of consumed stmts, which would
-- include everything CStar silently skipped.
data CMatcherState = CMatcherState
  { remaining    :: [CStmt]
  , boundExprs   :: HashMap Text CExpr
  , matchedAnns  :: [AddrRange]
  , matchedStmts :: [CStmt]
  } deriving (Show, Generic)

type CMatcherT m = StateT CMatcherState (LogicT m)

-- | Result of running a check: a bounded list of unique findings, plus a
-- flag indicating whether more unique findings existed than were returned.
-- Callers render the 'truncated' flag as a warning so users know the scan
-- was capped.
data CheckResult = CheckResult
  { findings  :: [CAstFinding]
  , truncated :: Bool
  } deriving (Eq, Show, Generic)

-- | Run a check against a list of C statements. Results are deduplicated
-- (matchers like 'CStar' produce many equivalent match states for a single
-- real finding) and capped at 'maxFindingsPerCheck'. Callers that need to
-- know whether truncation occurred should use 'runCheckCapped' instead.
runCheck :: CAstCheck -> [CStmt] -> [CAstFinding]
runCheck check = findings . runCheckCapped check

-- | Like 'runCheck' but also reports whether the cap was hit.
runCheckCapped :: CAstCheck -> [CStmt] -> CheckResult
runCheckCapped check stmts =
  let results = runCheckOnStmts (check ^. #pattern_) stmts
      rawFindings = fmap (toFinding check) results
      (shown, wasTruncated) = takeUniqueUpTo maxFindingsPerCheck rawFindings
  in CheckResult { findings = shown, truncated = wasTruncated }

-- | Collect up to 'n' unique items from a (possibly lazy) list, returning the
-- collected prefix and a flag indicating whether the cap was reached before
-- the list was exhausted.
takeUniqueUpTo :: Hashable a => Int -> [a] -> ([a], Bool)
takeUniqueUpTo cap = go HashSet.empty 0 []
  where
    go _ _ acc [] = (reverse acc, False)
    go seen n acc (x:xs)
      | n >= cap             = (reverse acc, True)
      | HashSet.member x seen = go seen n acc xs
      | otherwise             = go (HashSet.insert x seen) (n + 1) (x : acc) xs

-- | Run a statement pattern against a list of C statements.
-- Matches at the top level AND inside every nested body (if/for/while/
-- switch/block), so patterns like @CSequence [CStar, target]@ find the
-- target regardless of how deeply it's nested in control flow. Without
-- this, patterns only see the function's top-level stmts and miss
-- anything wrapped in an @if@ or loop.
--
-- Results are bounded by 'rawObservationLimit' as a safety bound on the
-- underlying LogicT stream; caller-visible deduping and the user-facing
-- cap live in 'runCheckCapped'.
runCheckOnStmts :: CStmtPat -> [CStmt] -> [CMatcherState]
runCheckOnStmts pat stmts =
  let action = matchStmtPat pat
      runAt stmtList =
        let initState = CMatcherState stmtList HashMap.empty [] []
        in fmap snd (observeMany rawObservationLimit (runStateT action initState))
  in take rawObservationLimit (concatMap runAt (allStmtLists stmts))

-- | Every stmt list reachable in an AST — the top level plus every nested
-- body inside control-flow constructs. Used by 'runCheckOnStmts' so a
-- pattern written as "scan this function for X" also finds X inside @if@,
-- loops, switch cases, and bare blocks.
allStmtLists :: [CStmt] -> [[CStmt]]
allStmtLists stmts = stmts : concatMap nestedLists stmts
  where
    nestedLists = \case
      CIf _ _ body       -> allStmtLists body
      CIfElse _ _ tb eb  -> allStmtLists tb <> allStmtLists eb
      CFor _ _ _ _ body  -> allStmtLists body
      CWhile _ _ body    -> allStmtLists body
      CDoWhile _ body _  -> allStmtLists body
      CSwitch _ _ cases  -> concatMap caseLists cases
      CBlock _ ss        -> allStmtLists ss
      _                   -> []
    caseLists (CCase _ ss)  = allStmtLists ss
    caseLists (CDefault ss) = allStmtLists ss

-- | Convert a match state to a finding. The rendered snippet is the
-- actually-matched stmts, not the consumed prefix.
toFinding :: CAstCheck -> CMatcherState -> CAstFinding
toFinding check ms = CAstFinding
  { findingName = check ^. #checkName
  , description = check ^. #description
  , severity = check ^. #severity
  , location = ms ^. #matchedAnns
  , cSource = renderStmts 0 (ms ^. #matchedStmts)
  , boundNames = HashMap.map renderExpr (ms ^. #boundExprs)
  }


-- ---------------------------------------------------------------------------
-- Expression matching
-- ---------------------------------------------------------------------------

-- | Match an expression pattern against a concrete CExpr.
matchExprPat :: Monad m => CExprPat -> CExpr -> CMatcherT m ()
matchExprPat pat expr = case pat of
  CWild -> pure ()

  CBind name inner -> do
    matchExprPat inner expr
    modify' $ \s -> s { boundExprs = HashMap.insert name expr (boundExprs s) }
    collectAnn (exprAnn expr)

  CBound name -> do
    bound <- gets (HashMap.lookup name . boundExprs)
    case bound of
      Just bExpr -> guard (exprEq bExpr expr)
      Nothing    -> empty  -- unbound name, fail

  CUses name -> do
    bound <- gets (HashMap.lookup name . boundExprs)
    case bound of
      Just bExpr -> guard (exprContains bExpr expr)
      Nothing    -> empty

  CNamePat n -> case expr of
    CIdent _ t -> guard (t == n)
    _          -> empty

  CNameRegex regex -> case expr of
    CIdent _ t -> guard (t =~ regex :: Bool)
    _          -> empty

  CLitIntPat -> case expr of
    CLitInt _ _ -> pure ()
    _           -> empty

  CLitStringPat -> case expr of
    CLitString _ _ -> pure ()
    _              -> empty

  CCallPat name argPats -> case expr of
    CFuncall _ fn args
      | fn == name && length args == length argPats ->
        zipWithM_ matchExprPat argPats args
    _ -> empty

  CCallNamesPat names argPats -> case expr of
    CFuncall _ fn args
      | HashSet.member fn names ->
        if null argPats then pure ()
        -- Prefix match: require at least as many args as patterns,
        -- match leading args, ignore trailing ones.
        else if length args >= length argPats then zipWithM_ matchExprPat argPats args
        else empty
    _ -> empty

  CIndexPat arrPat idxPat -> case expr of
    CIndex _ arr idx -> matchExprPat arrPat arr >> matchExprPat idxPat idx
    _ -> empty

  CArrowPat basePat fieldName -> case expr of
    CArrow _ base f -> guard (f == fieldName) >> matchExprPat basePat base
    _ -> empty

  CDotPat basePat fieldName -> case expr of
    CDot _ base f -> guard (f == fieldName) >> matchExprPat basePat base
    _ -> empty

  CBinOpPat op lhsPat rhsPat -> case expr of
    CBinaryOp _ o lhs rhs -> guard (o == op) >> matchExprPat lhsPat lhs >> matchExprPat rhsPat rhs
    _ -> empty

  CAnyCmpPat lhsPat rhsPat -> case expr of
    CBinaryOp _ op lhs rhs
      | op `elem` ["<", "<=", ">", ">=", "==", "!="] ->
        matchExprPat lhsPat lhs >> matchExprPat rhsPat rhs
    _ -> empty

  CAssignPat lhsPat rhsPat -> case expr of
    CAssign _ _ lhs rhs -> matchExprPat lhsPat lhs >> matchExprPat rhsPat rhs
    _ -> empty

  CUnaryPat op innerPat -> case expr of
    CUnaryOp _ o inner -> guard (o == op) >> matchExprPat innerPat inner
    _ -> empty

  CCastPat typeRegex innerPat -> case expr of
    CCast _ typeName inner -> guard (typeName =~ typeRegex :: Bool) >> matchExprPat innerPat inner
    _ -> empty

  CContains inner -> matchExprContains inner expr

  CNotPat inner -> do
    st <- get
    let results = observeMany 1 (runStateT (matchExprPat inner expr) st)
    guard (null results)

  COrPat a b -> matchExprPat a expr <|> matchExprPat b expr

  CAndPat a b -> matchExprPat a expr >> matchExprPat b expr

-- | Recursively search all subexpressions for a match.
matchExprContains :: Monad m => CExprPat -> CExpr -> CMatcherT m ()
matchExprContains pat expr =
  matchExprPat pat expr <|> searchChildren
  where
    searchChildren = case expr of
      CBinaryOp _ _ l r   -> matchExprContains pat l <|> matchExprContains pat r
      CUnaryOp _ _ e      -> matchExprContains pat e
      CPostfixOp _ _ e    -> matchExprContains pat e
      CFuncall _ _ args   -> asum (fmap (matchExprContains pat) args)
      CAssign _ _ l r     -> matchExprContains pat l <|> matchExprContains pat r
      CIndex _ a i        -> matchExprContains pat a <|> matchExprContains pat i
      CDot _ base _       -> matchExprContains pat base
      CArrow _ base _     -> matchExprContains pat base
      CCast _ _ inner     -> matchExprContains pat inner
      CCond _ c t f       -> matchExprContains pat c <|> matchExprContains pat t <|> matchExprContains pat f
      _                   -> empty


-- ---------------------------------------------------------------------------
-- Statement matching
-- ---------------------------------------------------------------------------

-- | Match a statement pattern against the current remaining statements.
matchStmtPat :: Monad m => CStmtPat -> CMatcherT m ()
matchStmtPat = \case
  CStmtExprPat exprPat -> do
    s <- nextStmt
    case s of
      CExprStmt _ e -> matchExprPat exprPat e >> recordMatchedStmt s
      _             -> empty

  CVarDeclPat typeRegex nameRegex mInitPat -> do
    s <- nextStmt
    case s of
      CVarDecl _ ty nm mInit -> do
        guard (ty =~ typeRegex :: Bool)
        guard (nm =~ nameRegex :: Bool)
        case (mInitPat, mInit) of
          (Just initPat, Just initExpr) -> matchExprPat initPat initExpr
          (Nothing, _)                  -> pure ()
          (Just _, Nothing)             -> empty  -- wanted init but none present
        recordMatchedStmt s
      _ -> empty

  CIfPat condPat bodyPats -> do
    s <- nextStmt
    case s of
      CIf _ cond body -> do
        matchExprPat condPat cond
        matchInBody bodyPats body
        recordMatchedStmt s
      CIfElse _ cond thenBody _ -> do
        matchExprPat condPat cond
        matchInBody bodyPats thenBody
        recordMatchedStmt s
      _ -> empty

  CForPat forPat -> do
    s <- nextStmt
    case s of
      CFor _ initC condC incrC body -> do
        matchForInit (forPat ^. #initPat) initC
        matchMaybePat (forPat ^. #condPat) condC
        matchMaybePat (forPat ^. #incrPat) incrC
        matchInBody (forPat ^. #bodyPat) body
        recordMatchedStmt s
      _ -> empty

  CWhilePat condPat bodyPats -> do
    s <- nextStmt
    case s of
      CWhile _ cond body -> do
        matchExprPat condPat cond
        matchInBody bodyPats body
        recordMatchedStmt s
      _ -> empty

  CDoWhilePat bodyPats condPat -> do
    s <- nextStmt
    case s of
      CDoWhile _ body cond -> do
        matchInBody bodyPats body
        matchExprPat condPat cond
        recordMatchedStmt s
      _ -> empty

  CAnyLoopPat bodyPats -> do
    s <- nextStmt
    case s of
      CFor _ _ _ _ body   -> matchInBody bodyPats body >> recordMatchedStmt s
      CWhile _ _ body     -> matchInBody bodyPats body >> recordMatchedStmt s
      CDoWhile _ body _   -> matchInBody bodyPats body >> recordMatchedStmt s
      _                    -> empty

  CReturnPat exprPat -> do
    s <- nextStmt
    case s of
      CReturn _ (Just e) -> matchExprPat exprPat e >> recordMatchedStmt s
      _                  -> empty

  CStar -> do
    -- Skip 0 or more statements: try matching 0, then 1, then 2, ...
    pure () <|> (void nextStmt >> matchStmtPat CStar)

  CSequence pats -> mapM_ matchStmtPat pats

  CBodyContains inner -> do
    st <- get
    guard (bodyContainsMatchWith (boundExprs st) inner (remaining st))

  CStmtOr a b -> matchStmtPat a <|> matchStmtPat b

  CStmtNot inner -> do
    st <- get
    let results = observeMany 1 (runStateT (matchStmtPat inner) st)
    guard (null results)

  CAvoidUntil avoid target -> matchAvoidUntil avoid target


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Consume the next statement from remaining.
nextStmt :: CMatcherT m CStmt
nextStmt = do
  st <- get
  case remaining st of
    []     -> empty
    (s:ss) -> put (st { remaining = ss }) >> return s

-- | Collect address annotations from a matched node.
collectAnn :: CAnn -> CMatcherT m ()
collectAnn ann = modify' $ \s -> s { matchedAnns = matchedAnns s <> ann }

-- | Record a statement that actually satisfied the pattern at this nesting
-- level, and collect its annotations. Only called from the stmt-level cases
-- in 'matchStmtPat' — not from 'CStar', which silently skips.
recordMatchedStmt :: CStmt -> CMatcherT m ()
recordMatchedStmt s = modify' $ \st -> st
  { matchedAnns  = matchedAnns st <> stmtAnn s
  , matchedStmts = matchedStmts st <> [s]
  }

-- | Match for-loop init clause.
-- For assignments (i = 0), matches the pattern against the LHS variable
-- so that CBind "loopVar" CWild captures the loop variable, not the whole assignment.
-- For declarations (int i = 0), matches against a synthetic CIdent for the variable name.
matchForInit :: Monad m => Maybe CExprPat -> CForInit -> CMatcherT m ()
matchForInit Nothing _ = pure ()
matchForInit (Just pat) initC = case initC of
  CForInitExpr (Just (CAssign _ _ lhs _)) -> matchExprPat pat lhs
  CForInitExpr (Just e)                   -> matchExprPat pat e
  CForInitExpr Nothing                    -> empty
  CForInitDecl _ nm _                     -> matchExprPat pat (CIdent [] nm)

-- | Match optional expression with optional pattern.
matchMaybePat :: Monad m => Maybe CExprPat -> Maybe CExpr -> CMatcherT m ()
matchMaybePat Nothing _           = pure ()
matchMaybePat (Just _) Nothing    = empty
matchMaybePat (Just pat) (Just e) = matchExprPat pat e

-- | Run body patterns inside a nested statement list.
-- Creates a sub-matcher that operates on the body, preserving bound names.
matchInBody :: [CStmtPat] -> [CStmt] -> CMatcherT m ()
matchInBody [] _ = pure ()
matchInBody pats body = do
  st <- get
  let bodySt = st { remaining = body }
      action = mapM_ matchStmtPat pats
      results = observeMany 50 (runStateT action bodySt)
  case results of
    []         -> empty
    ((_, s):_) -> put $ st
      { boundExprs = boundExprs s
      , matchedAnns = matchedAnns s
      }

-- | Check if a statement pattern matches anywhere in a nested block structure,
-- carrying forward any bound expressions from the enclosing match.
bodyContainsMatchWith :: HashMap Text CExpr -> CStmtPat -> [CStmt] -> Bool
bodyContainsMatchWith binds pat stmts =
  let initSt = CMatcherState stmts binds [] []
      -- Try matching with CStar prefix at current level
      action = matchStmtPat CStar >> matchStmtPat pat
      results :: [((), CMatcherState)]
      results = observeMany 1 (runStateT action initSt)
  in not (null results) || any (stmtContainsWith binds pat) stmts

-- | Recursively search into nested statement blocks.
stmtContainsWith :: HashMap Text CExpr -> CStmtPat -> CStmt -> Bool
stmtContainsWith binds pat stmt = case stmt of
  CIf _ _ body           -> bodyContainsMatchWith binds pat body
  CIfElse _ _ tb eb      -> bodyContainsMatchWith binds pat tb || bodyContainsMatchWith binds pat eb
  CFor _ _ _ _ body      -> bodyContainsMatchWith binds pat body
  CWhile _ _ body        -> bodyContainsMatchWith binds pat body
  CDoWhile _ body _      -> bodyContainsMatchWith binds pat body
  CSwitch _ _ cases      -> any caseContains cases
  CBlock _ ss            -> bodyContainsMatchWith binds pat ss
  _                      -> False
  where
    caseContains (CCase _ ss)  = bodyContainsMatchWith binds pat ss
    caseContains (CDefault ss) = bodyContainsMatchWith binds pat ss

-- | AvoidUntil: scan forward, failing if the avoid pattern matches before
-- the target pattern matches. Checks avoid FIRST so that a statement matching
-- both avoid and target is treated as avoid (fails).
matchAvoidUntil :: Monad m => CStmtPat -> CStmtPat -> CMatcherT m ()
matchAvoidUntil avoid target = go
  where
    go = do
      st <- get
      -- First: if avoid matches, fail immediately
      let avoidResults = observeMany 1 (runStateT (matchStmtPat avoid) st)
      if not (null avoidResults)
        then empty
        else
          -- Try target, or skip one statement and recurse
          matchStmtPat target <|> (void nextStmt >> go)


-- ---------------------------------------------------------------------------
-- Expression equality and containment
-- ---------------------------------------------------------------------------

-- | Structural equality of expressions, deliberately ignoring CAnn
-- (address range annotations). Two expressions at different addresses
-- are considered equal if they have the same structure and leaf values.
-- This differs from the derived Eq instance, which compares annotations.
--
-- NOTE: if CExpr gains new constructors, this catch-all returns False.
-- Keep in sync with CExpr in Ghidra.Clang.
exprEq :: CExpr -> CExpr -> Bool
exprEq (CIdent _ a) (CIdent _ b)                   = a == b
exprEq (CLitInt _ a) (CLitInt _ b)                  = a == b
exprEq (CLitString _ a) (CLitString _ b)            = a == b
exprEq (CBinaryOp _ o1 l1 r1) (CBinaryOp _ o2 l2 r2) = o1 == o2 && exprEq l1 l2 && exprEq r1 r2
exprEq (CUnaryOp _ o1 e1) (CUnaryOp _ o2 e2)       = o1 == o2 && exprEq e1 e2
exprEq (CFuncall _ n1 a1) (CFuncall _ n2 a2)        = n1 == n2 && length a1 == length a2 && and (zipWith exprEq a1 a2)
exprEq (CIndex _ a1 i1) (CIndex _ a2 i2)           = exprEq a1 a2 && exprEq i1 i2
exprEq (CArrow _ b1 f1) (CArrow _ b2 f2)           = f1 == f2 && exprEq b1 b2
exprEq (CDot _ b1 f1) (CDot _ b2 f2)               = f1 == f2 && exprEq b1 b2
exprEq _ _                                          = False

-- | Check if a target expression appears anywhere in a host expression tree.
exprContains :: CExpr -> CExpr -> Bool
exprContains target host
  | exprEq target host = True
  | otherwise = case host of
      CBinaryOp _ _ l r  -> exprContains target l || exprContains target r
      CUnaryOp _ _ e     -> exprContains target e
      CPostfixOp _ _ e   -> exprContains target e
      CFuncall _ _ args  -> any (exprContains target) args
      CAssign _ _ l r    -> exprContains target l || exprContains target r
      CIndex _ a i       -> exprContains target a || exprContains target i
      CDot _ base _      -> exprContains target base
      CArrow _ base _    -> exprContains target base
      CCast _ _ inner    -> exprContains target inner
      CCond _ c t f      -> exprContains target c || exprContains target t || exprContains target f
      _                  -> False
