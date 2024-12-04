module Blaze.Cfg.Solver.BranchContext where

import Blaze.Prelude hiding (Constraint, Type, bitSize, sym)

import Blaze.Cfg qualified as Cfg
import Blaze.Cfg.Analysis qualified as CfgA
import Blaze.Cfg.Checker (checkCfg)
import Blaze.Graph qualified as G
import Blaze.Pil.Analysis qualified as PA
import Blaze.Pil.Solver (catchIfLenient, catchIfLenientForStmt, makeSymVarOfType, logError)
import Blaze.Pil.Solver qualified as PilSolver
import Blaze.Types.Cfg (
  BranchCond,
  BranchType (FalseBranch, TrueBranch),
  BranchingType (OnlyFalse, OnlyTrue, Undecided),
  CfEdge (CfEdge),
  CfNode,
  Cfg,
  PilCfg,
  PilNode,
  UndecidedIfBranches (UndecidedIfBranches),
 )
import Blaze.Types.Graph (DominatorMapping)
import Blaze.Types.Graph.Alga (AlgaGraph)
import Blaze.Types.Graph.EdgeGraph (EdgeGraphNode)
import Blaze.Types.Graph.EdgeGraph qualified as Eg
import Blaze.Types.Pil (PilVar)
import Blaze.Types.Pil qualified as Pil
import Blaze.Types.Pil.Analysis (DataDependenceGraph)
import Blaze.Types.Pil.Checker (DeepSymType, SymTypedStmt)
import Blaze.Types.Pil.Checker qualified as Ch
import Blaze.Types.Pil.Solver
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.SBV.Dynamic (SVal, svAnd, svNot)
import Data.SBV.Dynamic qualified as D hiding (Solver)
import Data.SBV.Trans qualified as SBV
import Data.SBV.Trans.Control qualified as Q


type TypedExpression = Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)

data DecidedBranchCond = DecidedBranchCond
  { conditionStatementIndex :: Int
  , condition :: TypedExpression
  , decidedBranch :: Bool
  } deriving (Eq, Ord, Show, Generic)

data GeneralSolveError = TypeCheckerError Ch.ConstraintGenError
                       | SolverError Ch.TypeReport PilSolver.SolverError
                       deriving (Eq, Ord, Show, Generic)


-- | If the node is a conditional if-node, and one of the branches has been removed,
-- this returns which branch remains (True or False) and the conditional expr.
getDecidedBranchCondNode
  :: CfNode [(Int, SymTypedStmt)]
  -> Cfg (CfNode [(Int, SymTypedStmt)])
  -> Maybe DecidedBranchCond
getDecidedBranchCondNode n cfg = case outBranchTypes of
  [] -> Nothing
  [TrueBranch] -> mcond ?? True
  [FalseBranch] -> mcond ?? False
  [TrueBranch, FalseBranch] -> Nothing
  [FalseBranch, TrueBranch] -> Nothing
  _ -> Nothing
  where
    mcond = lastMay (Cfg.getNodeData n) >>= \case
      (i, Pil.Stmt _ (Pil.BranchCond (Pil.BranchCondOp x))) -> Just $ DecidedBranchCond i x
      _ -> Nothing

    outBranchTypes :: [BranchType]
    outBranchTypes = fmap (view #branchType)
      . HashSet.toList
      $ Cfg.succEdges n cfg

-- | Adds overly general constraints for a CFG. Unpruned If conditions are ignored.
-- Phi vars are Or'd together. TODO memory
generalCfgFormula
  :: DataDependenceGraph
  -> Cfg (CfNode [(Int, SymTypedStmt)])
  -> Solver ()
generalCfgFormula ddg cfg = do
  mapM_ setIndexAndSolveStmt $ Cfg.gatherCfgData cfg
  where
    setIndexAndSolveStmt :: (Int, SymTypedStmt) -> Solver ()
    setIndexAndSolveStmt (i, stmt) = do
      #currentStmtIndex .= i
      catchIfLenientForStmt $ solveStmt ddg stmt

solveStmt
  :: DataDependenceGraph
  -> SymTypedStmt
  -> Solver ()
solveStmt ddg stmt@(Pil.Stmt _ statement) = case statement of
  -- this is handled elsewhere (only used if there is single outgoing branch edge)
  Pil.BranchCond _ -> return ()
  -- TODO: convert stores/loads into immutable SSA vars
  Pil.Store _ -> return ()
  Pil.DefPhi (Pil.DefPhiOp dest vars) -> unless (any isDependentOn vars) pilSolveStmt
    where
      destDescendants = G.getStrictDescendants dest ddg
      isDependentOn = flip HashSet.member destDescendants
  _ -> pilSolveStmt
  where
    pilSolveStmt = PilSolver.solveStmt_ (solveExpr ddg) stmt

solveExpr :: DataDependenceGraph -> DSTExpression -> Solver SVal
solveExpr ddg expr@(Ch.InfoExpression (Ch.SymInfo _sz xsym, mdst) op) = catchFallbackAndWarn $ case op of
  -- TOOD: turn mem into immutable phi vars
  Pil.LOAD _ -> fallbackAsFreeVar
  _ -> PilSolver.solveExpr_ (solveExpr ddg) expr
  where
    fallbackAsFreeVar :: Solver SVal
    fallbackAsFreeVar = case mdst of
      Nothing -> throwError . ExprError xsym . ErrorMessage $ "missing DeepSymType"
      Just dst -> catchError (makeSymVarOfType Nothing dst) $ \e ->
          throwError $ ExprError xsym e

    catchFallbackAndWarn :: Solver SVal -> Solver SVal
    catchFallbackAndWarn m = catchError m $ \e -> do
      si <- use #currentStmtIndex
      logError $ StmtError si e
      fallbackAsFreeVar

----------------------------

getBranchCondNodes
  :: Cfg (CfNode [(Int, SymTypedStmt)])
  -> [BranchCond (CfNode [(Int, SymTypedStmt)]) TypedExpression]
getBranchCondNodes = Cfg.getBranchCondNodes (first Just)

toSolvedBranchCondNode
  :: DataDependenceGraph
  -> BranchCond (CfNode [(Int, SymTypedStmt)]) TypedExpression
  -> Solver (BranchCond (CfNode [(Int, SymTypedStmt)]) SVal)
toSolvedBranchCondNode ddg bnode = do
  whenJust (bnode ^. #conditionStatementIndex) (#currentStmtIndex .=)
  traverse solveExprOrMakeAmbiguous bnode
  where
    solveExprOrMakeAmbiguous :: TypedExpression -> Solver SVal
    solveExprOrMakeAmbiguous expr = catchIfLenient identity (solveExpr ddg expr)
      . const $ D.svNewVar_ D.KBool

getSolvedBranchCondNodes
  :: DataDependenceGraph
  -> Cfg (CfNode [(Int, SymTypedStmt)])
  -> Solver [BranchCond (CfNode [(Int, SymTypedStmt)]) SVal]
getSolvedBranchCondNodes ddg =
  traverse (toSolvedBranchCondNode ddg) . getBranchCondNodes

data UndecidedBranchCond a = UndecidedBranchCond
  { conditionStatementIndex :: Int
  , selfNode :: CfNode a
  , condition :: TypedExpression
  , trueEdge :: CfEdge (CfNode a)
  , falseEdge :: CfEdge (CfNode a)
  } deriving (Eq, Ord, Show, Generic)

-- | If the node is a conditional if-node, and one of the branches has been removed,
-- this returns which branch remains (True or False) and the conditional expr.
getUndecidedBranchCondNode
  :: CfNode [(Int, SymTypedStmt)]
  -> Cfg (CfNode [(Int, SymTypedStmt)])
  -> Maybe (UndecidedBranchCond [(Int, SymTypedStmt)])
getUndecidedBranchCondNode n cfg = Cfg.getOutBranchingType n cfg >>= \case
  Undecided UndecidedIfBranches {falseEdge = fe, trueEdge = te} ->
    mcond <*> pure te <*> pure fe
  OnlyTrue _ -> Nothing
  OnlyFalse _ -> Nothing
  where
    mcond = lastMay (Cfg.getNodeData n) >>= \case
      (i, Pil.Stmt _ (Pil.BranchCond (Pil.BranchCondOp x))) -> Just $ UndecidedBranchCond i n x
      _ -> Nothing

-- | Returns mapping of nodes to contextual constraints of unpruned if-nodes.
-- Ignores decided/pruned if-nodes because they will be part of full general-formula
getBranchContextConstraints
  :: Cfg (CfNode [(Int, SymTypedStmt)])
  -> [UndecidedBranchCond [(Int, SymTypedStmt)]]
  -> HashMap (CfNode [(Int, SymTypedStmt)]) [TypedExpression]
getBranchContextConstraints typedCfg = buildHashMap . concatMap getConstraintsForUndecidedBranches
  where
    buildHashMap :: Hashable k => [(k, v)] -> HashMap k [v]
    buildHashMap = foldl' (\m (k, v) -> HashMap.insertWith (<>) k [v] m) HashMap.empty

    getConstraintsForUndecidedBranches :: UndecidedBranchCond [(Int, SymTypedStmt)]
                                       -> [(CfNode [(Int, SymTypedStmt)], TypedExpression)]
    getConstraintsForUndecidedBranches u = trueNodeConstraints <> falseNodeConstraints
      where
        -- TODO: Can't make it with expressions because we don't have checker var syms.
        -- need to use solver monad

        trueNodeConstraints :: [(CfNode [(Int, SymTypedStmt)], TypedExpression)]
        trueNodeConstraints = fmap (, u ^. #condition) trueUnique
        falseNodeConstraints :: [(CfNode [(Int, SymTypedStmt)], TypedExpression)]
        falseNodeConstraints = fmap (, mkNot $ u ^. #condition) falseUnique
        mkNot :: TypedExpression -> TypedExpression
        mkNot x = Ch.InfoExpression (x ^. #info)
                  (Pil.NOT . Pil.NotOp $ x)
        getReachable :: CfNode [(Int, SymTypedStmt)] -> HashSet (CfNode [(Int, SymTypedStmt)])
        getReachable n = HashSet.fromList . G.reachable n . view #graph $ typedCfg

        -- If branch node can reach its parent, then it is looping and is discarded.
        -- TODO: this removes too many nodes.
        --       Ideally, it would return all the nodes uniquely reachable by the branch node
        --       up until it actually loops back.
        getNonLoopingReachable ::
          CfNode [(Int, SymTypedStmt)] ->
          CfNode [(Int, SymTypedStmt)] ->
          HashSet (CfNode [(Int, SymTypedStmt)])
        getNonLoopingReachable srcNode dstNode =
          let reached = getReachable dstNode in
            bool HashSet.empty reached $ HashSet.member srcNode reached
        falseReachable :: HashSet (CfNode [(Int, SymTypedStmt)])
        falseReachable = getNonLoopingReachable (u ^. #falseEdge . #src) (u ^. #falseEdge . #dst)
        trueReachable :: HashSet (CfNode [(Int, SymTypedStmt)])
        trueReachable = getNonLoopingReachable (u ^. #trueEdge . #src) (u ^. #trueEdge . #dst)
        falseUnique :: [CfNode [(Int, SymTypedStmt)]]
        falseUnique = HashSet.toList $ HashSet.difference falseReachable trueReachable
        trueUnique :: [CfNode [(Int, SymTypedStmt)]]
        trueUnique = HashSet.toList $ HashSet.difference trueReachable falseReachable

filterEdges ::
  forall a m. (Hashable a, DominatorMapping m) =>
  m (EdgeGraphNode BranchType (CfNode a)) ->
  m (CfEdge (CfNode a))
filterEdges = G.domMapMaybe f
 where
  f ::
    EdgeGraphNode BranchType (CfNode a) ->
    Maybe (CfEdge (CfNode a))
  f (Eg.NodeNode _) = Nothing
  f (Eg.EdgeNode e) = Just $ Cfg.fromLEdge e

mkEdgeConstraintMap
  :: [BranchCond (CfNode [(Int, SymTypedStmt)]) SVal]
  -> HashMap (CfEdge (CfNode [(Int, SymTypedStmt)])) SVal
mkEdgeConstraintMap = HashMap.fromList . concatMap f
  where
    f bc = case bc ^. #branchingType of
      OnlyTrue e -> [(e, bc ^. #condition)]
      OnlyFalse e -> [(e, svNot $ bc ^. #condition)]
      Undecided UndecidedIfBranches {falseEdge = fe, trueEdge = te} ->
        [ (fe, svNot $ bc ^. #condition)
        , (te, bc ^. #condition)
        ]

{- | Checks all undecided branch nodes in depth-first order.
 ignores branch nodes in loops
 Check for each branch edge consists of dominator edge constraints
 Results should be used iteratively with function that actually deletes unsat branches
-}
unsatBranches ::
  DataDependenceGraph ->
  Cfg (CfNode [(Int, SymTypedStmt)]) ->
  Solver [CfEdge (CfNode [(Int, SymTypedStmt)])]
unsatBranches ddg cfg = do
  svalBranchCondNodes <- getSolvedBranchCondNodes ddg cfg
  case mapMaybe getUndecided svalBranchCondNodes of
    [] -> return []
    ubranches -> do
      let edgeConstraints = mkEdgeConstraintMap svalBranchCondNodes
          edgeGraph :: AlgaGraph () Int (EdgeGraphNode BranchType (CfNode [(Int, SymTypedStmt)]))
          edgeGraph = Eg.toEdgeGraph $ cfg ^. #graph
          eRoot = Eg.NodeNode $ Cfg.getRootNode cfg
          doms = filterEdges $ G.getDominators eRoot edgeGraph

          _domConstraintsDebug = fmap (mapMaybe (\e -> HashMap.lookup e edgeConstraints >> return e) . HashSet.toList) . coerce $ doms :: HashMap (CfEdge (CfNode [(Int, SymTypedStmt)])) [CfEdge (CfNode [(Int, SymTypedStmt)])]
          domConstraints = fmap (mapMaybe (`HashMap.lookup` edgeConstraints) . HashSet.toList) . coerce $ doms :: HashMap (CfEdge (CfNode [(Int, SymTypedStmt)])) [SVal]

      generalCfgFormula ddg cfg
      er <- liftSymbolicT . Q.query $ do
        -- Make sure General formula works
        er' <-
          Q.checkSat >>= \case
            Q.DSat _ -> return $ Right ()
            Q.Sat -> return $ Right ()
            r -> Left <$> toSolverResult r
        case er' of
          Left r -> return $ Left r
          Right () -> do
            xxs <- forM ubranches $ \(bcond, UndecidedIfBranches fe te) -> do
              let commonDomConstraints = PilSolver.svAggrAnd . fromMaybe [] $ HashMap.lookup fe domConstraints

              feResult <- tryConstraint $ commonDomConstraints `svAnd` svNot bcond
              teResult <- tryConstraint $ commonDomConstraints `svAnd` bcond

              return $ case (feResult, teResult) of
                -- (False, False) means the whole branch context is unsat
                -- and it should get removed higher up in the Cfg.
                (False, False) -> [fe, te]
                (False, True) -> [fe]
                (True, False) -> [te]
                (True, True) -> []
            return . Right . concat $ xxs

      case er of
        Left sr -> do
          putText $ "General constraints are not sat: " <> show sr
          return []
        Right xs -> return xs
 where
  getUndecided ::
    BranchCond (CfNode [(Int, SymTypedStmt)]) SVal ->
    Maybe (SVal, UndecidedIfBranches (CfNode [(Int, SymTypedStmt)]))
  getUndecided bc = case bc ^. #branchingType of
    OnlyTrue _ -> Nothing
    OnlyFalse _ -> Nothing
    Undecided edges -> Just (bc ^. #condition, edges)

tryConstraint :: SVal -> Query Bool
tryConstraint c = Q.inNewAssertionStack
  $ SBV.constrain (PilSolver.toSBool' c) >> isSat
  where
    isSat :: Query Bool
    isSat = Q.checkSat >>= \case
      Q.DSat _ -> return True
      Q.Sat -> return True
      _ -> return False


-- | A type report and list of warnings is generated every step of simplification.
data WarnReport = WarnReport
  { warnings :: [SolverError]
  , typeReport :: Ch.TypeReport
  } deriving (Eq, Show, Generic)

-- TODO: Need to recover original edges. The single use of this function only uses the original edges
getUnsatBranches :: Cfg PilNode -> IO (Either GeneralSolveError (WarnReport, [CfEdge PilNode]))
getUnsatBranches cfg = case checkCfg cfg of
  -- The TypeCheckerError only occurs if the type checker cannot produce a type report.
  -- The type report could still report errors.
  Left err -> return . Left . TypeCheckerError $ err
  Right (_, cfg', tr) -> do
    let ddg = CfgA.getDataDependenceGraph cfg
    er <- flip (PilSolver.runSolverWith SBV.z3)
          ( PilSolver.emptyState
          , SolverCtx (tr ^. #varSymTypeMap) HashMap.empty False IgnoreErrors
          )
          $ PilSolver.declarePilVars >> unsatBranches ddg cfg'
    case er of
      Left err -> return . Left $ SolverError tr err
      Right (unsatEdges, ss) ->
        return $ Right ( WarnReport (ss ^. #errors) tr
                       , fmap getOrigEdge unsatEdges
                       )
   where
     getOrigEdge :: CfEdge (CfNode [(Int, SymTypedStmt)]) -> CfEdge PilNode
     getOrigEdge (CfEdge src dst label) =
       let src' = fromJust $ Cfg.getNode cfg (G.getNodeId src)
           dst' = fromJust $ Cfg.getNode cfg (G.getNodeId dst) in
             CfEdge src' dst' label


_cleanPrunedCfg :: Int -> PilCfg -> PilCfg
_cleanPrunedCfg numItersLeft cfg =
  if cfg == cfg'' || numItersLeft <= 0
    then cfg''
    else -- Recursing until stmts don't change or no iterations left
      _cleanPrunedCfg (numItersLeft - 1) cfg''
 where
  cfg' :: PilCfg
  cfg' = CfgA.copyProp cfg
  -- Need deadNodes to compute removedVars and to actually remove the dead nodes
  deadNodes :: HashSet PilNode
  deadNodes = CfgA.getDeadNodes cfg'
  removedVars :: HashSet PilVar
  removedVars = PA.getDefinedVars (concatMap Cfg.getNodeData deadNodes)
  cfg'' :: PilCfg
  cfg'' =
    CfgA.reducePhi removedVars
      . CfgA.fixed CfgA.removeUnusedPhi
      . CfgA.removeNodes deadNodes
      $ cfg'

cleanPrunedCfg :: PilCfg -> PilCfg
cleanPrunedCfg = CfgA.removeEmptyBasicBlockNodes . _cleanPrunedCfg maxIters
 where
  maxIters = 10

simplify_ :: [WarnReport] -> Bool -> Int -> PilCfg -> IO (Either GeneralSolveError ([WarnReport], PilCfg))
simplify_ warns isRecursiveCall numItersLeft cfg
  | numItersLeft <= 0 = return . Right $ (warns, cfg)
  | otherwise = do
      let cfg' = cleanPrunedCfg cfg
      if cfg' == cfg && isRecursiveCall
        then return . Right $ (warns, cfg')
        else getUnsatBranches cfg' >>= \case
          Left err -> return $ Left err
          Right (wr, []) -> return . Right $ (wr:warns, cfg')
          Right (wr, es) -> simplify_ (wr:warns) True (numItersLeft - 1)
                      $ foldr Cfg.removeEdge cfg' es

simplify :: PilCfg -> IO (Either GeneralSolveError ([WarnReport], PilCfg))
simplify stmts = do
  simplify_ [] False 10 stmts
