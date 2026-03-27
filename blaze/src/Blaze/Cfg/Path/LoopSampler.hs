module Blaze.Cfg.Path.LoopSampler
  ( module Blaze.Cfg.Path.LoopSampler
  ) where

import Blaze.Prelude hiding (Symbol, const, succ)

import Blaze.Types.Cfg (Cfg, PilNode, BranchType(FalseBranch, UnconditionalBranch))
import qualified Blaze.Cfg as Cfg
import Blaze.Types.Cfg.Path (Path)
import qualified Blaze.Types.Cfg.Path as CfgPath
import Blaze.Types.Graph (StrictDescendantsMap)
import qualified Blaze.Types.Graph as G
import Blaze.Path (VisitCounts, getVisitCount, updateVisitCounts,
                   stochasticChoiceFp, getStrictDescendants)
import qualified Blaze.Types.Path as P
import Blaze.Types.Pil (Stmt, Expression(Expression), PilVar)
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Pil.Analysis as PA
import qualified Blaze.Pil.Construct as C
import Blaze.Types.Pil.Analysis.Subst (RecurSubst(recurSubst))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List (span)


-----------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------

-- | A detected linear induction variable from a loop header phi node.
data InductionVar = InductionVar
  { phiDest   :: !PilVar   -- ^ phi result variable (e.g. x#1)
  , initVal   :: !PilVar   -- ^ pre-loop operand (e.g. x#0)
  , updateVal :: !PilVar   -- ^ in-loop update operand (e.g. x#2)
  , stride    :: !Int64     -- ^ constant stride (+1, -1, etc.)
  , loopVar   :: !PilVar   -- ^ fresh symbolic variable (x_loop)
  } deriving (Eq, Ord, Show, Generic)

-- | Comparison operator extracted from the loop guard.
data BoundOp
  = BoundSLT  -- ^ signed <
  | BoundSLE  -- ^ signed <=
  | BoundSGT  -- ^ signed >
  | BoundSGE  -- ^ signed >=
  | BoundULT  -- ^ unsigned <
  | BoundULE  -- ^ unsigned <=
  | BoundUGT  -- ^ unsigned >
  | BoundUGE  -- ^ unsigned >=
  | BoundNE   -- ^ !=
  deriving (Eq, Ord, Show, Generic)

-- | Bound information extracted from the loop condition.
data BoundInfo = BoundInfo
  { boundExpr :: !Expression
  , boundOp   :: !BoundOp
  } deriving (Eq, Ord, Show, Generic)

-- | Errors that can occur during loop-summarizing sampling.
data SamplerError
  = StartNodeNotInGraph
  | NoChildrenReachable
  deriving (Eq, Ord, Show, Generic)

-- | Internal sampler accumulator state.
data LoopSamplerState = LoopSamplerState
  { visitedNodes :: !(HashSet PilNode)
  , pathAcc      :: ![(BranchType, PilNode)]  -- ^ reverse order (most recent first)
  , reqSeq       :: ![PilNode]
  , visitCounts  :: !(VisitCounts PilNode)
  , nextLoopCtxId :: !Pil.CtxId               -- ^ next context ID to allocate for loops
  } deriving (Eq, Ord, Show, Generic)


-----------------------------------------------------------------------
-- Main entry point
-----------------------------------------------------------------------

-- | Sample a path from a cyclic CFG with loop summarization.
--   When a backedge is detected, the loop body is summarized into a
--   single abstract iteration rather than being unrolled.
sampleWithLoopSummarization
  :: forall m. Monad m
  => m Double                         -- ^ random double [0,1) for stochastic choice
  -> StrictDescendantsMap PilNode
  -> [PilNode]                        -- ^ required sequence of nodes to visit
  -> VisitCounts PilNode              -- ^ accumulated visit counts (for diversity)
  -> PilNode                          -- ^ start node
  -> Cfg PilNode                      -- ^ cyclic CFG (backedges intact)
  -> m (Either SamplerError (Path PilNode, VisitCounts PilNode))
sampleWithLoopSummarization randDouble dmap reqSeq0 visitCounts0 startNode cfg
  | not (G.hasNode startNode cfg) = return $ Left StartNodeNotInGraph
  | otherwise = do
      let initState = LoopSamplerState
            { visitedNodes = HashSet.empty
            , pathAcc = []
            , reqSeq = reqSeq0
            , visitCounts = visitCounts0
            , nextLoopCtxId = cfg ^. #nextCtxIndex + 1
            }
      result <- walk startNode Nothing initState
      return $ case result of
        Left err -> Left err
        Right st -> case buildPathFromAcc (st ^. #nextLoopCtxId) (st ^. #pathAcc) of
          Nothing -> Left NoChildrenReachable
          Just path -> Right (path, st ^. #visitCounts)
  where
    graph = cfg ^. #graph

    -- | Get successors of a node as (BranchType, PilNode) pairs.
    getSuccs :: PilNode -> [(BranchType, PilNode)]
    getSuccs n = fmap f . HashSet.toList $ G.succEdges n graph
      where f e = (e ^. #label, e ^. #edge . #dst)

    -- | Main walk loop. Accumulates nodes in reverse order.
    walk
      :: PilNode                     -- ^ current node
      -> Maybe BranchType            -- ^ edge label from previous node (Nothing for start)
      -> LoopSamplerState
      -> m (Either SamplerError LoopSamplerState)
    walk currentNode mLabel st = do
      let lbl = fromMaybe UnconditionalBranch mLabel
          st' = st
            & #visitedNodes %~ HashSet.insert currentNode
            & #pathAcc %~ ((lbl, currentNode) :)
            & #visitCounts %~ updateVisitCounts currentNode
            & #reqSeq %~ advanceSeq currentNode

      -- If all required sequence nodes have been visited, we're done
      if null (st' ^. #reqSeq)
        then return $ Right st'
        else do
          let children = getSuccs currentNode
              remainingReqs = HashSet.fromList (st' ^. #reqSeq)
              validChildren = filterReachable dmap remainingReqs children
          case validChildren of
            [] -> return $ Right st'  -- dead end, return what we have
            cs' -> do
              chosen <- pickChild randDouble dmap (st' ^. #visitCounts) cs'
              if HashSet.member (snd chosen) (st' ^. #visitedNodes)
                then handleBackedge (snd chosen) (fst chosen) st'
                else walk (snd chosen) (Just $ fst chosen) st'

    -- | Handle a detected backedge to a loop header.
    handleBackedge
      :: PilNode                     -- ^ the loop header (backedge target)
      -> BranchType                  -- ^ the edge label of the backedge
      -> LoopSamplerState
      -> m (Either SamplerError LoopSamplerState)
    handleBackedge header _backEdgeLabel st = do
      let (loopBodyFwd, prePath) = splitLoopBody header (st ^. #pathAcc)
          preLoopVars = getDefinedVarsFromAcc prePath
          bodyNodeSet = HashSet.fromList $ fmap snd loopBodyFwd

      case detectInductionVar header loopBodyFwd preLoopVars of
        Nothing -> do
          -- Fallback: keep one iteration as-is, try to continue from exit
          continueFromExit header bodyNodeSet st

        Just indVar -> do
          let loopCtxId = st ^. #nextLoopCtxId
              bound = extractBound (indVar ^. #phiDest) header
              hdrAddr = getNodeAddr header
              hdrCtx = getNodeCtx header
              summaryStmts = buildLoopSummary indVar bound hdrAddr hdrCtx loopCtxId loopBodyFwd
              -- Use the header's UUID for the summary node (deterministic, no collision
              -- since the header is replaced). This ensures nub can dedup identical paths.
              summaryNode = mkSummaryNode header (Cfg.getNodeUUID header) summaryStmts
              -- Replace the loop body in pathAcc with the summary node
              exitLabel = FalseBranch  -- exit is typically the false branch
              st' = st
                & #pathAcc .~ ((exitLabel, summaryNode) : prePath)
                & #nextLoopCtxId %~ (+ 1)
          continueFromExit header bodyNodeSet st'

    -- | After handling a loop, continue sampling from the exit edge.
    continueFromExit
      :: PilNode                     -- ^ loop header
      -> HashSet PilNode             -- ^ loop body node set
      -> LoopSamplerState
      -> m (Either SamplerError LoopSamplerState)
    continueFromExit header bodyNodeSet st =
      case findExitEdge header bodyNodeSet of
        Nothing -> return $ Right st  -- no exit, end the path here
        Just (exitLbl, exitNode) -> walk exitNode (Just exitLbl) st

    -- | Find the exit edge from the loop header (the successor NOT in the loop body).
    findExitEdge :: PilNode -> HashSet PilNode -> Maybe (BranchType, PilNode)
    findExitEdge header bodyNodeSet =
      let headerSuccs = getSuccs header
      in find (\(_, n) -> not (HashSet.member n bodyNodeSet)) headerSuccs


-----------------------------------------------------------------------
-- Induction variable detection
-----------------------------------------------------------------------

-- | Attempt to detect a linear induction variable from the loop header's phi nodes.
detectInductionVar
  :: PilNode                           -- ^ loop header node
  -> [(BranchType, PilNode)]           -- ^ loop body (forward order, starting with header)
  -> HashSet PilVar                    -- ^ variables defined before the loop
  -> Maybe InductionVar
detectInductionVar header loopBody preLoopVars =
  listToMaybe $ mapMaybe tryPhi phis
  where
    headerStmts = Cfg.getNodeData header
    phis = [(dest, srcs) | Pil.Stmt _ (Pil.DefPhi (Pil.DefPhiOp dest srcs)) <- headerStmts]
    bodyDefinedVars = getDefinedVarsFromNodes loopBody

    tryPhi :: (PilVar, [PilVar]) -> Maybe InductionVar
    tryPhi (dest, srcs) = do
      (initV, updateV) <- classifySrcs srcs
      stride' <- findLinearStride dest updateV loopBody
      let loopV = dest
            & #symbol %~ (<> "_looping")
            & #version .~ Nothing
      return InductionVar
        { phiDest = dest
        , initVal = initV
        , updateVal = updateV
        , stride = stride'
        , loopVar = loopV
        }

    classifySrcs :: [PilVar] -> Maybe (PilVar, PilVar)
    classifySrcs [a, b]
      | HashSet.member a bodyDefinedVars && not (HashSet.member a preLoopVars)
        = Just (b, a)  -- b is init, a is update
      | HashSet.member b bodyDefinedVars && not (HashSet.member b preLoopVars)
        = Just (a, b)  -- a is init, b is update
    classifySrcs _ = Nothing

-- | Search the loop body for a definition of @updateVar@ that is
--   @phiDest +/- constant@.
findLinearStride
  :: PilVar                            -- ^ phi destination variable
  -> PilVar                            -- ^ update variable
  -> [(BranchType, PilNode)]           -- ^ loop body (forward)
  -> Maybe Int64
findLinearStride phiDest updateVar loopBody =
  listToMaybe . mapMaybe checkStmt $ allBodyStmts loopBody
  where
    checkStmt :: Stmt -> Maybe Int64
    checkStmt (Pil.Stmt _ (Pil.Def (Pil.DefOp dest expr)))
      | dest == updateVar = extractLinearStride phiDest expr
    checkStmt _ = Nothing

-- | Check if an expression is of the form @v + C@, @v - C@,
--   or @ARRAY_ADDR(v, C, stride)@ where @v@ is the given variable
--   and @C@ is a constant.
extractLinearStride :: PilVar -> Expression -> Maybe Int64
extractLinearStride v (Expression _ op) = case op of
  Pil.ADD (Pil.AddOp left right)
    | isVarRef v left  -> getConstVal right
    | isVarRef v right -> getConstVal left
  Pil.SUB (Pil.SubOp left right)
    | isVarRef v left  -> negate <$> getConstVal right
  Pil.ARRAY_ADDR (Pil.ArrayAddrOp base idx stride)
    | isVarRef v base -> do
        c <- getConstVal idx
        let effective = signExtend32 c * fromIntegral stride
        return effective
  _ -> Nothing

-- | Sign-extend an Int64 value that may have been zero-extended from 32-bit.
--   Values like 0xFFFFFFFF (4294967295) should be interpreted as -1.
signExtend32 :: Int64 -> Int64
signExtend32 x
  | x > 0x7FFFFFFF && x <= 0xFFFFFFFF = x - 0x100000000
  | otherwise = x


-----------------------------------------------------------------------
-- Bound extraction
-----------------------------------------------------------------------

-- | Extract the loop bound from the header's branch condition or constraints.
--   Handles both inline comparisons (?: x < 100) and flag-variable patterns
--   (cf = x < 100; ?: cf) by resolving through definitions.
extractBound :: PilVar -> PilNode -> Maybe BoundInfo
extractBound indVar header =
  listToMaybe . mapMaybe (matchBound indVar) $ resolvedConditions
  where
    headerStmts = Cfg.getNodeData header

    -- Build a map from PilVar -> defining Expression for this block
    defMap :: HashMap PilVar Expression
    defMap = HashMap.fromList
      [ (dest, val)
      | Pil.Stmt _ (Pil.Def (Pil.DefOp dest val)) <- headerStmts
      ]

    -- Resolve a condition: if it's just a VAR reference, look up its definition
    resolve :: Expression -> Expression
    resolve expr@(Expression _ (Pil.VAR (Pil.VarOp pv))) =
      fromMaybe expr $ HashMap.lookup pv defMap
    resolve expr = expr

    rawConditions :: [Expression]
    rawConditions
       = [cond | Pil.Stmt _ (Pil.BranchCond (Pil.BranchCondOp cond)) <- headerStmts]
      <> [cond | Pil.Stmt _ (Pil.Constraint (Pil.ConstraintOp cond)) <- headerStmts]

    resolvedConditions :: [Expression]
    resolvedConditions = fmap resolve rawConditions

-- | Match a condition expression against known comparison patterns
--   where one operand is the induction variable.
matchBound :: PilVar -> Expression -> Maybe BoundInfo
matchBound indVar (Expression _ op) = case op of
  -- indVar < bound
  Pil.CMP_SLT (Pil.CmpSltOp left right)
    | isVarRef indVar left  -> Just $ BoundInfo right BoundSLT
    | isVarRef indVar right -> Just $ BoundInfo left BoundSGT
  -- indVar <= bound
  Pil.CMP_SLE (Pil.CmpSleOp left right)
    | isVarRef indVar left  -> Just $ BoundInfo right BoundSLE
    | isVarRef indVar right -> Just $ BoundInfo left BoundSGE
  -- indVar > bound
  Pil.CMP_SGT (Pil.CmpSgtOp left right)
    | isVarRef indVar left  -> Just $ BoundInfo right BoundSGT
    | isVarRef indVar right -> Just $ BoundInfo left BoundSLT
  -- indVar >= bound
  Pil.CMP_SGE (Pil.CmpSgeOp left right)
    | isVarRef indVar left  -> Just $ BoundInfo right BoundSGE
    | isVarRef indVar right -> Just $ BoundInfo left BoundSLE
  -- Unsigned variants
  Pil.CMP_ULT (Pil.CmpUltOp left right)
    | isVarRef indVar left  -> Just $ BoundInfo right BoundULT
    | isVarRef indVar right -> Just $ BoundInfo left BoundUGT
  Pil.CMP_ULE (Pil.CmpUleOp left right)
    | isVarRef indVar left  -> Just $ BoundInfo right BoundULE
    | isVarRef indVar right -> Just $ BoundInfo left BoundUGE
  Pil.CMP_UGT (Pil.CmpUgtOp left right)
    | isVarRef indVar left  -> Just $ BoundInfo right BoundUGT
    | isVarRef indVar right -> Just $ BoundInfo left BoundULT
  Pil.CMP_UGE (Pil.CmpUgeOp left right)
    | isVarRef indVar left  -> Just $ BoundInfo right BoundUGE
    | isVarRef indVar right -> Just $ BoundInfo left BoundULE
  -- Not-equal (common for counted loops)
  Pil.CMP_NE (Pil.CmpNeOp left right)
    | isVarRef indVar left  -> Just $ BoundInfo right BoundNE
    | isVarRef indVar right -> Just $ BoundInfo left BoundNE
  _ -> Nothing


-----------------------------------------------------------------------
-- Loop summary construction
-----------------------------------------------------------------------

-- | Build the summarized statement list for a loop.
--   1. Entry constraints placing v_loop in the valid range
--   2. Loop body with indvar substituted to v_loop, minus phi/update/branch
--   3. Exit constraint (negation of loop condition)
buildLoopSummary
  :: InductionVar
  -> Maybe BoundInfo
  -> Address                          -- ^ header address (used for constraint stmt addresses)
  -> Pil.Ctx                          -- ^ outer context (from header node)
  -> Pil.CtxId                        -- ^ unique context ID for this loop
  -> [(BranchType, PilNode)]         -- ^ loop body (forward, starting with header)
  -> [Stmt]
buildLoopSummary indVar mBound headerAddr outerCtx loopCtxId loopBody =
  [enterLoopStmt] <> entryConstraints <> rewrittenBody <> exitConstraints <> [exitLoopStmt]
  where
    loopCtx :: Pil.Ctx
    loopCtx = outerCtx & #isLoopCtx .~ True & #ctxId .~ loopCtxId

    enterLoopStmt :: Stmt
    enterLoopStmt = C.mkStmtAt headerAddr . Pil.EnterContext $ Pil.EnterContextOp loopCtx []

    exitLoopStmt :: Stmt
    exitLoopStmt = C.mkStmtAt headerAddr . Pil.ExitContext $ Pil.ExitContextOp loopCtx outerCtx

    sz :: Pil.Size Expression
    sz = fromByteBased $ indVar ^. #loopVar . #size

    -- The looping and exit vars carry the loop context
    loopVarWithCtx :: PilVar
    loopVarWithCtx = (indVar ^. #loopVar) & #ctx ?~ loopCtx

    loopVarExpr :: Expression
    loopVarExpr = C.var' loopVarWithCtx sz

    initExpr :: Expression
    initExpr = C.var' (indVar ^. #initVal) sz

    -- Entry constraints: v_loop is within [init, bound) or similar
    entryConstraints :: [Stmt]
    entryConstraints = initBound <> upperBound
      where
        initBound
          | indVar ^. #stride > 0
            = [C.constraintAt headerAddr $ C.cmpSge loopVarExpr initExpr sz]
          | otherwise
            = [C.constraintAt headerAddr $ C.cmpSle loopVarExpr initExpr sz]
        upperBound = case mBound of
          Nothing -> []
          Just bi -> [C.constraintAt headerAddr $ mkBoundConstraint loopVarExpr (bi ^. #boundExpr) (bi ^. #boundOp) sz]

    -- Rewritten body: substitute phiDest -> loopVar, remove phi/update/branch
    rewrittenBody :: [Stmt]
    rewrittenBody
      = fmap (recurSubst substVar)
      . filter (not . shouldRemove)
      $ allBodyStmts loopBody

    substVar :: PilVar -> PilVar
    substVar pv
      | pv == indVar ^. #phiDest = loopVarWithCtx
      | otherwise = pv

    shouldRemove :: Stmt -> Bool
    shouldRemove (Pil.Stmt _ stmt) = case stmt of
      Pil.DefPhi (Pil.DefPhiOp dest _) -> dest == indVar ^. #phiDest
      Pil.Def (Pil.DefOp dest _) -> dest == indVar ^. #updateVal
      Pil.BranchCond _ -> True  -- remove the loop branch condition
      _ -> False

    -- Exit linkage: define phiDest as the exit value so post-loop code
    -- that references it gets a meaningful constrained value instead of
    -- the pre-loop init (which copy-prop would otherwise propagate).
    exitConstraints :: [Stmt]
    exitConstraints = case mBound of
      Nothing -> []
      Just bi ->
        let exitVar = (indVar ^. #phiDest)
              & #symbol %~ (<> "_exit")
              & #version .~ Nothing
              & #ctx ?~ loopCtx
            exitExpr = C.var' exitVar sz
        in [ C.constraintAt headerAddr $ mkExitConstraint exitExpr (bi ^. #boundExpr) (bi ^. #boundOp) sz
           , C.mkStmtAt headerAddr . Pil.Def $ Pil.DefOp (indVar ^. #phiDest) exitExpr  -- phiDest = exitVar
           ]

-- | Build a constraint expression for the loop bound: @v_loop OP bound@.
mkBoundConstraint :: Expression -> Expression -> BoundOp -> Pil.Size Expression -> Expression
mkBoundConstraint lhs rhs = \case
  BoundSLT -> C.cmpSlt lhs rhs
  BoundSLE -> C.cmpSle lhs rhs
  BoundSGT -> C.cmpSgt lhs rhs
  BoundSGE -> C.cmpSge lhs rhs
  BoundULT -> C.cmpUlt lhs rhs
  BoundULE -> C.cmpUle lhs rhs
  BoundUGT -> C.cmpUgt lhs rhs
  BoundUGE -> C.cmpUge lhs rhs
  BoundNE  -> C.cmpNE  lhs rhs

-- | Build the exit constraint (negation of the loop condition).
--   If loop continues while @v < bound@, exit when @v >= bound@.
mkExitConstraint :: Expression -> Expression -> BoundOp -> Pil.Size Expression -> Expression
mkExitConstraint lhs rhs = \case
  BoundSLT -> C.cmpSge lhs rhs
  BoundSLE -> C.cmpSgt lhs rhs
  BoundSGT -> C.cmpSle lhs rhs
  BoundSGE -> C.cmpSlt lhs rhs
  BoundULT -> C.cmpUge lhs rhs
  BoundULE -> C.cmpUgt lhs rhs
  BoundUGT -> C.cmpUle lhs rhs
  BoundUGE -> C.cmpUlt lhs rhs
  BoundNE  -> C.cmpE   lhs rhs


-----------------------------------------------------------------------
-- Path construction
-----------------------------------------------------------------------

-- | Build a PilPath from the accumulated (reversed) node list.
buildPathFromAcc :: Pil.CtxId -> [(BranchType, PilNode)] -> Maybe (Path PilNode)
buildPathFromAcc _ [] = Nothing
buildPathFromAcc ctxId revAcc =
  let fwd = reverse revAcc
  in case fwd of
    [] -> Nothing
    ((_, startNode) : edges) ->
      let pb = foldl' (\pb' (lbl, n) -> pb' P.-| lbl P.|- n) (P.start startNode) edges
      in Just $ CfgPath.build ctxId pb


-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

-- | Create a summary BasicBlock node for the loop.
mkSummaryNode :: PilNode -> UUID -> [Stmt] -> PilNode
mkSummaryNode headerNode uuid stmts =
  Cfg.BasicBlock $ Cfg.BasicBlockNode
    { ctx = case headerNode of
        Cfg.BasicBlock bb -> bb ^. #ctx
        Cfg.Call cn -> cn ^. #ctx
        Cfg.EnterFunc ef -> ef ^. #nextCtx
        Cfg.LeaveFunc lf -> lf ^. #nextCtx
        Cfg.Grouping _ -> error "mkSummaryNode: unexpected Grouping node"
    , start = headerAddr
    , end = headerAddr
    , uuid = uuid
    , nodeData = stmts
    }
  where
    headerAddr = case headerNode of
      Cfg.BasicBlock bb -> bb ^. #start
      Cfg.Call cn -> cn ^. #start
      _ -> intToAddr 0

-- | Split the reversed pathAcc at the loop header, returning
--   (loop body in forward order, pre-loop in reverse order).
splitLoopBody
  :: PilNode
  -> [(BranchType, PilNode)]           -- ^ pathAcc (reversed)
  -> ( [(BranchType, PilNode)]         -- ^ loop body (forward: header first, tail last)
     , [(BranchType, PilNode)]         -- ^ pre-loop path (still reversed)
     )
splitLoopBody header revAcc =
  let (bodyAfterHeaderRev, headerAndPre) = span (\(_, n) -> n /= header) revAcc
  in case headerAndPre of
    [] -> ([], revAcc)  -- header not found (shouldn't happen)
    (headerEntry : prePath) ->
      (headerEntry : reverse bodyAfterHeaderRev, prePath)

-- | Advance the required sequence if the current node matches the next expected node.
advanceSeq :: PilNode -> [PilNode] -> [PilNode]
advanceSeq _ [] = []
advanceSeq current (req : rest)
  | current == req = rest
  | otherwise = req : rest

-- | Collect all statements from a list of (label, node) pairs.
allBodyStmts :: [(BranchType, PilNode)] -> [Stmt]
allBodyStmts = concatMap (Cfg.getNodeData . snd)

-- | Get all variables defined in a list of path nodes.
getDefinedVarsFromNodes :: [(BranchType, PilNode)] -> HashSet PilVar
getDefinedVarsFromNodes = PA.getDefinedVars . allBodyStmts

-- | Get all variables defined in the (reversed) path accumulator.
getDefinedVarsFromAcc :: [(BranchType, PilNode)] -> HashSet PilVar
getDefinedVarsFromAcc = getDefinedVarsFromNodes

-- | Check if an expression is a VAR reference to the given PilVar.
isVarRef :: PilVar -> Expression -> Bool
isVarRef v (Expression _ (Pil.VAR (Pil.VarOp v'))) = v == v'
isVarRef _ _ = False

-- | Extract an Int64 constant from an expression.
getConstVal :: Expression -> Maybe Int64
getConstVal (Expression _ (Pil.CONST (Pil.ConstOp c))) = Just c
getConstVal _ = Nothing

-- | Get the start address from a CfNode.
getNodeAddr :: PilNode -> Address
getNodeAddr (Cfg.BasicBlock bb) = bb ^. #start
getNodeAddr (Cfg.Call cn) = cn ^. #start
getNodeAddr _ = intToAddr 0

-- | Get the Ctx from a CfNode.
getNodeCtx :: PilNode -> Pil.Ctx
getNodeCtx (Cfg.BasicBlock bb) = bb ^. #ctx
getNodeCtx (Cfg.Call cn) = cn ^. #ctx
getNodeCtx (Cfg.EnterFunc ef) = ef ^. #nextCtx
getNodeCtx (Cfg.LeaveFunc lf) = lf ^. #nextCtx
getNodeCtx (Cfg.Grouping _) = error "getNodeCtx: unexpected Grouping node"

-- | Filter children to those that can reach all remaining required nodes.
filterReachable
  :: StrictDescendantsMap PilNode
  -> HashSet PilNode                   -- ^ remaining required nodes
  -> [(BranchType, PilNode)]           -- ^ candidate children
  -> [(BranchType, PilNode)]
filterReachable _ reqs children
  | HashSet.null reqs = children
filterReachable dmap reqs children =
  filter canReach children
  where
    canReach (_, n) = case getStrictDescendants dmap n of
      Nothing -> False
      Just descs ->
        let reachable = HashSet.insert n descs
        in reqs `HashSet.isSubsetOf` reachable

-- | Stochastically pick a child weighted by descendant count,
--   adjusted by visit counts for diversity.
pickChild
  :: Monad m
  => m Double
  -> StrictDescendantsMap PilNode
  -> VisitCounts PilNode
  -> [(BranchType, PilNode)]
  -> m (BranchType, PilNode)
pickChild randDouble dmap vc children =
  case children of
    [x] -> return x
    (x:xs) -> do
      let rated = fmap rateChild (x :| xs)
      stochasticChoiceFp randDouble rated
    [] -> error "pickChild: empty children list"
  where
    rateChild :: (BranchType, PilNode) -> (Double, (BranchType, PilNode))
    rateChild child@(_, n) =
      let descCount = case getStrictDescendants dmap n of
            Nothing -> 1
            Just s -> fromIntegral (HashSet.size s) + 1
          visitPenalty = 1.0 / fromIntegral (getVisitCount n vc + 1)
      in (descCount * visitPenalty, child)


-----------------------------------------------------------------------
-- IO convenience
-----------------------------------------------------------------------

sampleWithLoopSummarizationIO
  :: StrictDescendantsMap PilNode
  -> [PilNode]
  -> VisitCounts PilNode
  -> PilNode
  -> Cfg PilNode
  -> IO (Either SamplerError (Path PilNode, VisitCounts PilNode))
sampleWithLoopSummarizationIO = sampleWithLoopSummarization randomIO
