{- HLINT ignore "Use if" -}
module Blaze.Pil.Checker where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)
import Binja.Function (Function)
import Blaze.Types.Pil ( Expression
                       , Statement
                       , PilVar
                       )
import qualified Blaze.Types.Pil as Pil
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Blaze.Types.Pil.Checker hiding (symStmts)
import Blaze.Types.Pil.Function (FuncVar)
import Blaze.Pil.Checker.Constraints ( addStmtTypeConstraints )
import Blaze.Pil.Checker.Unification ( unify )
import Blaze.Pil.Analysis ( originMapToGroupMap )
import qualified Blaze.Pil as Pil
import qualified Blaze.Pil.Analysis as Analysis


flatToDeepSyms :: HashMap Sym (PilType Sym) -> HashMap Sym DeepSymType
flatToDeepSyms flatSymHm = HashMap.mapWithKey (parseF HashSet.empty) flatSymHm
  where
    substSym :: HashSet Sym -> Sym -> DeepSymType
    substSym hs s = case HashMap.lookup s flatSymHm of
      Nothing -> DSVar s
      Just v -> parseF hs s v
    substSymRecurse :: HashSet Sym -> Sym -> Sym -> DeepSymType
    substSymRecurse hs og s = case s == og of
      True -> DSVar s
      False -> case HashMap.lookup s flatSymHm of
        Nothing -> DSVar s
        Just v -> parseFRecurse hs s v
    parseFRecurse :: HashSet Sym -> Sym -> PilType Sym -> DeepSymType
    parseFRecurse sSyms sym' ptS = case HashSet.member sym' sSyms of
      True -> DSVar sym'
      False -> DSType $ fmap (substSymRecurse (HashSet.insert sym' sSyms) sym') ptS
    parseF :: HashSet Sym -> Sym -> PilType Sym -> DeepSymType
    parseF sSyms sym' ptS = case HashSet.member sym' sSyms of
      True -> DSVar sym'
      -- TODO: bubble up seen syms of children with DeepSymType rather than
      -- calling subSyms at every level
      False -> case HashSet.member sym' $ subSyms HashSet.empty flatSymHm sym' of
        True -> DSRecursive sym' $ fmap (substSymRecurse (HashSet.insert sym' sSyms) sym') ptS
        False -> DSType $ fmap (substSym $ HashSet.insert sym' sSyms) ptS

subSyms :: HashSet Sym -> HashMap Sym (PilType Sym) -> Sym -> HashSet Sym
subSyms sSyms hm s = case HashSet.member s sSyms of
  True -> HashSet.fromList [s]
  False -> case HashMap.lookup s hm of
    Nothing -> HashSet.empty
    Just v -> HashSet.unions . map (subSyms (HashSet.insert s sSyms) hm) $ foldr (:) [] v

unifyConstraints :: [Constraint] -> UnifyState
unifyConstraints cxs = snd $ runUnify unify initialState
  where
    initialState = UnifyState { _constraints = cxs
                              , _solutions = HashMap.empty
                              , _errors = []
                              , _originMap = HashMap.empty
                              , _currentStmt = 0
                              }

-- | Adds constraints for all statements.
-- This is a good place to add constraints that require
-- access to all statements.
addAllConstraints ::
  [(Int, Statement Expression)] ->
  Either ConstraintGenError
    ( [Statement SymExpression],
      ConstraintGenState
    )
addAllConstraints indexedStmts = 
  fmap (, gst) er
  where
    er :: Either ConstraintGenError [Statement SymExpression]
    gst :: ConstraintGenState
    (er, gst) = runConstraintGen_ $ do
      mapM
        ( \(i, s) -> do
            currentStmt .= i
            addStmtTypeConstraints s
        )
        indexedStmts

stmtSolutions ::
  [(Int, Statement Expression)] ->
  Either ConstraintGenError
    ( [Statement SymExpression],
      ConstraintGenState,
      UnifyState
    )
stmtSolutions indexedStmts = do
  (symStmts, genState) <- addAllConstraints indexedStmts
  let unifyState = unifyConstraints . reverse $ genState ^. constraints
  return (fmap (varSubst $ unifyState ^. originMap) symStmts,
          genState,
          unifyState)

-- | main function to type check / infer statements
--   currently only returning types of pilvars in stmts for testing.
checkIndexedStmts :: [(Int, Statement Expression)] -> Either ConstraintGenError TypeReport
checkIndexedStmts indexedStmts = fmap toReport . stmtSolutions $ indexedStmts
  where
    toReport :: ( [Statement SymExpression]
                , ConstraintGenState
                , UnifyState
                )
             -> TypeReport
    toReport (stmts', s, unSt) = TypeReport
      { _symTypeStmts = zip (fmap fst indexedStmts) $ fmap (fmap fillTypesInStmt) stmts'
      , _symStmts = stmts'
      , _varSymMap = originsVarSymMap
      , _varSymTypeMap = pilVarMap
      , _varEqMap = originMapToGroupMap eqMap
      , _funcSymTypeMap = funcVarMap
      , _funcSymMap = originsFuncVarSymMap
      , _errors = errs
      , _flatSolutions = sols
      , _solutions = deepSols
      }
      where
        originsVarSymMap :: HashMap PilVar Sym
        originsVarSymMap = varSubst eqMap <$> s ^. varSymMap
        sols :: HashMap Sym (PilType Sym)
        sols = unSt ^. solutions
        errs :: [UnifyConstraintsError DeepSymType]
        errs = fmap f <$> unSt ^. errors
          where
            f s' = maybe (DSVar s') identity $ HashMap.lookup s' deepSols
        eqMap :: HashMap Sym Sym
        eqMap = unSt ^. originMap
        deepSols :: HashMap Sym DeepSymType
        deepSols = flatToDeepSyms sols
        fillTypesInStmt :: InfoExpression SymInfo
                        -> InfoExpression (SymInfo, Maybe DeepSymType)
        fillTypesInStmt x = InfoExpression
          ( x ^. info
          , do
              originSym <- HashMap.lookup (x ^. info . sym) eqMap
              HashMap.lookup originSym deepSols
          )
          (fmap fillTypesInStmt $ x ^. op)
        pilVarMap :: HashMap PilVar DeepSymType
        pilVarMap = fmap f originsVarSymMap
          where
            f :: Sym -> DeepSymType
            f sv = maybe (DSVar sv) identity $ HashMap.lookup sv deepSols
        originsFuncVarSymMap :: HashMap (FuncVar SymExpression) Sym
        originsFuncVarSymMap = varSubst eqMap <$> s ^. funcSymMap
        funcVarMap :: HashMap (FuncVar SymExpression) DeepSymType
        funcVarMap = fmap f originsFuncVarSymMap
          where
            f :: Sym -> DeepSymType
            f sv = maybe (DSVar sv) identity $ HashMap.lookup sv deepSols

checkStmts :: [Statement Expression] -> Either ConstraintGenError TypeReport
checkStmts = checkIndexedStmts . zip [0..]

removeUnusedPhi :: [(Int, Pil.Stmt)] -> [(Int, Pil.Stmt)]
removeUnusedPhi stmts' = filter (not . Analysis.isUnusedPhi refs . view _2) stmts'
  where
    refs = Analysis.getRefVars . fmap snd $ stmts'

-- TODO: Consider introducing an IndexedStmt type to avoid the awkwardness
--       below where we split a [(Int, Stmt)] to process all [Stmt] and then
--       reassemble.
checkFunction :: Function -> IO (Either ConstraintGenError TypeReport)
checkFunction func = do
  indexedStmts <- Pil.fromFunction func
  return $ checkIndexedStmts . removeUnusedPhi $
    zip (fmap fst indexedStmts) (Analysis.substAddrs $ fmap snd indexedStmts)

