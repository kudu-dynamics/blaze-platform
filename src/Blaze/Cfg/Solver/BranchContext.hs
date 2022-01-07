module Blaze.Cfg.Solver.BranchContext where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)

import Blaze.Types.Pil (Statement, Stmt, PilVar)
import qualified Blaze.Types.Pil as Pil
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Blaze.Pil.Analysis as PA
import Blaze.Types.Pil.Checker (DeepSymType)
import Blaze.Types.Cfg (Cfg, CfNode, BranchType(TrueBranch, FalseBranch), CfEdge)
import Blaze.Types.Cfg.Interprocedural (InterCfg(InterCfg), unInterCfg)
import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Graph as G
import Blaze.Types.Graph (EdgeGraphNode, DominatorMapping)
import Blaze.Types.Pil.Analysis ( DataDependenceGraph )
import qualified Blaze.Cfg.Analysis as CfgA
import qualified Blaze.Pil.Solver as PilSolver
import Blaze.Pil.Solver (makeSymVarOfType, warn)
import Blaze.Types.Pil.Solver
import qualified Blaze.Types.Pil.Checker as Ch
import Blaze.Cfg.Checker (checkCfg)
import Data.SBV.Dynamic (SVal, svNot, svAnd)
import qualified Data.SBV.Trans.Control as Q
import qualified Data.SBV.Trans as SBV
import Blaze.Types.Graph.Alga (AlgaGraph)


type TypedExpression = Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)

data DecidedBranchCond = DecidedBranchCond
  { conditionStatementIndex :: Int
  , condition :: TypedExpression
  , decidedBranch :: Bool
  } deriving (Eq, Ord, Show, Generic)

data UndecidedIfBranches = UndecidedIfBranches
  { falseEdge :: CfEdge ()
  , trueEdge :: CfEdge ()
  } deriving (Eq, Ord, Show, Generic)

data BranchingType = OnlyTrue (CfEdge ())
                   | OnlyFalse (CfEdge ())
                   | Undecided UndecidedIfBranches
                   deriving (Eq, Ord, Show, Generic)

data BranchCond a = BranchCond
  { conditionStatementIndex :: Int
  , condition :: a
  , branchingType :: BranchingType
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data GeneralSolveError = TypeCheckerError Ch.ConstraintGenError
                       | SolverError Ch.TypeReport PilSolver.SolverError
                       deriving (Eq, Ord, Show, Generic)


-- | If the node is a conditional if-node, and one of the branches has been removed,
-- this returns which branch remains (True or False) and the conditional expr.
getDecidedBranchCondNode
  :: CfNode [(Int, Statement TypedExpression)]
  -> Cfg [(Int, Statement TypedExpression)]
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
      (i, Pil.BranchCond (Pil.BranchCondOp x)) -> Just $ DecidedBranchCond i x
      _ -> Nothing
      
    outBranchTypes :: [BranchType]
    outBranchTypes = fmap (view #branchType)
      . HashSet.toList
      $ Cfg.succEdges n cfg

-- | Adds overly general constraints for a CFG. Unpruned If conditions are ignored.
-- Phi vars are Or'd together. TODO memory
generalCfgFormula
  :: DataDependenceGraph
  -> Cfg [(Int, Statement TypedExpression)]
  -> Solver ()
generalCfgFormula ddg cfg = do
  mapM_ setIndexAndSolveStmt $ Cfg.gatherCfgData cfg
  where
    setIndexAndSolveStmt :: (Int, Statement TypedExpression) -> Solver ()
    setIndexAndSolveStmt (i, stmt) = do
      #currentStmtIndex .= i
      solveStmt ddg stmt

solveStmt
  :: DataDependenceGraph
  -> Statement TypedExpression
  -> Solver ()
solveStmt ddg stmt = case stmt of
  -- this is handled elsewhere (only used if there is single outgoing branch edge)
  Pil.BranchCond _ -> return ()
  -- TODO: convert stores/loads into immutable SSA vars
  Pil.Store _ -> return ()
  Pil.DefPhi (Pil.DefPhiOp dest vars) -> unless (or $ isDependentOn <$> vars) pilSolveStmt
    where
      destDescendants = G.getDescendants dest ddg
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
      warn $ StmtError si e
      fallbackAsFreeVar

----------------------------


-- | If the node is a conditional if-node, and one of the branches has been removed,
-- this returns which branch remains (True or False) and the conditional expr.
getBranchCondNode
  :: CfNode [(Int, Statement TypedExpression)]
  -> Cfg [(Int, Statement TypedExpression)]
  -> Maybe (BranchCond TypedExpression)
getBranchCondNode n cfg = mcond <*> getOutBranchingType n cfg  
  where
    mcond = lastMay (Cfg.getNodeData n) >>= \case
      (i, Pil.BranchCond (Pil.BranchCondOp x)) -> Just $ BranchCond i x
      _ -> Nothing

getOutBranchingType :: CfNode [(Int, Statement TypedExpression)] -> Cfg [(Int, Statement TypedExpression)] -> Maybe BranchingType
getOutBranchingType n cfg = case outBranches of
  [(TrueBranch, tedge), (FalseBranch, fedge)] ->
    Just . Undecided $ UndecidedIfBranches { falseEdge = fedge, trueEdge = tedge }
  [(FalseBranch, fedge), (TrueBranch, tedge)] ->
    Just . Undecided $ UndecidedIfBranches { falseEdge = fedge, trueEdge = tedge }
  [(TrueBranch, tedge)] ->
    Just $ OnlyTrue tedge
  [(FalseBranch, fedge)] ->
    Just $ OnlyFalse fedge
  _ -> Nothing
  where
    outBranches :: [(BranchType, CfEdge ())]
    outBranches =
      fmap (\e -> (e ^. #branchType, Cfg.asIdEdge e))
      . HashSet.toList
      $ Cfg.succEdges n cfg


getBranchCondNodes
  :: Cfg [(Int, Statement TypedExpression)]
  -> [BranchCond TypedExpression]
getBranchCondNodes typedCfg = mapMaybe (`getBranchCondNode` typedCfg)
  . HashSet.toList
  . G.nodes
  $ typedCfg

toSolvedBranchCondNode
  :: DataDependenceGraph
  -> BranchCond TypedExpression
  -> Solver (BranchCond SVal)
toSolvedBranchCondNode ddg bnode = do
  #currentStmtIndex .= bnode ^. #conditionStatementIndex
  traverse (solveExpr ddg) bnode

getSolvedBranchCondNodes
  :: DataDependenceGraph
  -> Cfg [(Int, Statement TypedExpression)]
  -> Solver [BranchCond SVal]
getSolvedBranchCondNodes ddg =
  traverse (toSolvedBranchCondNode ddg) . getBranchCondNodes

data UndecidedBranchCond = UndecidedBranchCond
  { conditionStatementIndex :: Int
  , selfNode :: CfNode ()
  , condition :: TypedExpression
  , trueEdge :: CfEdge ()
  , falseEdge :: CfEdge ()
  } deriving (Eq, Ord, Show, Generic)

-- | If the node is a conditional if-node, and one of the branches has been removed,
-- this returns which branch remains (True or False) and the conditional expr.
getUndecidedBranchCondNode
  :: CfNode [(Int, Statement TypedExpression)]
  -> Cfg [(Int, Statement TypedExpression)]
  -> Maybe UndecidedBranchCond
getUndecidedBranchCondNode n cfg = getOutBranchingType n cfg >>= \case
  Undecided UndecidedIfBranches {falseEdge = fe, trueEdge = te} ->
    mcond <*> pure te <*> pure fe
  OnlyTrue _ -> Nothing
  OnlyFalse _ -> Nothing
  where
    mcond = lastMay (Cfg.getNodeData n) >>= \case
      (i, Pil.BranchCond (Pil.BranchCondOp x)) -> Just $ UndecidedBranchCond i (Cfg.asIdNode n) x
      _ -> Nothing

-- | Returns mapping of nodes to contextual constraints of unpruned if-nodes.
-- Ignores decided/pruned if-nodes because they will be part of full general-formula
getBranchContextConstraints
  :: Cfg [(Int, Statement TypedExpression)]
  -> [UndecidedBranchCond]
  -> HashMap (CfNode ()) [TypedExpression]
getBranchContextConstraints typedCfg = buildHashMap . concatMap getConstraintsForUndecidedBranches
  where
    buildHashMap :: (Hashable k, Eq k) => [(k, v)] -> HashMap k [v]
    buildHashMap = foldl' (\m (k, v) -> HashMap.insertWith (<>) k [v] m) HashMap.empty

    getConstraintsForUndecidedBranches :: UndecidedBranchCond
                                       -> [(CfNode (), TypedExpression)]
    getConstraintsForUndecidedBranches u = trueNodeConstraints <> falseNodeConstraints
      where
        -- TODO: Can't make it with expressions because we don't have checker var syms.
        -- need to use solver monad

        trueNodeConstraints :: [(CfNode (), TypedExpression)]
        trueNodeConstraints = fmap (, u ^. #condition) trueUnique
        falseNodeConstraints :: [(CfNode (), TypedExpression)]
        falseNodeConstraints = fmap (, mkNot $ u ^. #condition) falseUnique
        mkNot :: TypedExpression -> TypedExpression
        mkNot x = Ch.InfoExpression (x ^. #info)
                  (Pil.NOT . Pil.NotOp $ x)
        getReachable :: CfNode () -> HashSet (CfNode ())
        getReachable n = HashSet.fromList . G.reachable n . view #graph $ typedCfg

        -- If branch node can reach its parent, then it is looping and is discarded.
        -- TODO: this removes too many nodes.
        --       Ideally, it would return all the nodes uniquely reachable by the branch node
        --       up until it actually loops back.
        getNonLoopingReachable :: CfNode () -> CfNode () -> HashSet (CfNode ())
        getNonLoopingReachable srcNode dstNode =
          let reached = getReachable dstNode in
            bool HashSet.empty reached $ HashSet.member srcNode reached
        falseReachable :: HashSet (CfNode ())    
        falseReachable = getNonLoopingReachable (u ^. #falseEdge . #src) (u ^. #falseEdge . #dst)
        trueReachable :: HashSet (CfNode ())
        trueReachable = getNonLoopingReachable (u ^. #trueEdge . #src) (u ^. #trueEdge . #dst)
        falseUnique :: [CfNode ()]
        falseUnique = HashSet.toList $ HashSet.difference falseReachable trueReachable
        trueUnique :: [CfNode ()]
        trueUnique = HashSet.toList $ HashSet.difference trueReachable falseReachable

filterEdges
  :: DominatorMapping m
  => m (EdgeGraphNode BranchType (CfNode ()))
  -> m (CfEdge ())
filterEdges = G.domMapMaybe f
  where
    f :: EdgeGraphNode BranchType (CfNode ()) -> Maybe (CfEdge ())
    f (G.NodeNode _) = Nothing
    f (G.EdgeNode e) = Just $ Cfg.fromLEdge e

mkEdgeConstraintMap
  :: [BranchCond SVal]
  -> HashMap (CfEdge ()) SVal
mkEdgeConstraintMap = HashMap.fromList . concatMap f
  where
    f bc = case bc ^. #branchingType of
      OnlyTrue e -> [(e, bc ^. #condition)]
      OnlyFalse e -> [(e, svNot $ bc ^. #condition)]
      Undecided UndecidedIfBranches {falseEdge = fe, trueEdge = te} ->
        [ (fe, svNot $ bc ^. #condition)
        , (te, bc ^. #condition)
        ]

-- | Checks all undecided branch nodes in depth-first order.
-- ignores branch nodes in loops
-- Check for each branch edge consists of dominator edge constraints
-- Results should be used iteratively with function that actually deletes unsat branches
unsatBranches
  :: DataDependenceGraph
  -> Cfg [(Int, Statement TypedExpression)]
  -> Solver [CfEdge ()]
unsatBranches ddg cfg = do
  svalBranchCondNodes <- getSolvedBranchCondNodes ddg cfg
  case mapMaybe getUndecided svalBranchCondNodes of
    [] -> return []
    ubranches -> do
      let edgeConstraints = mkEdgeConstraintMap svalBranchCondNodes
          edgeGraph = G.toEdgeGraph $ cfg ^. #graph :: AlgaGraph () () (EdgeGraphNode BranchType (CfNode ()))
          eRoot = G.NodeNode . Cfg.asIdNode $ cfg ^. #root
          doms = filterEdges $ G.getDominators eRoot edgeGraph
          
          _domConstraintsDebug = fmap (mapMaybe (\e -> HashMap.lookup e edgeConstraints >> return e) . HashSet.toList) . coerce $ doms :: HashMap (CfEdge ()) [CfEdge ()]
          domConstraints = fmap (mapMaybe (`HashMap.lookup` edgeConstraints) . HashSet.toList) . coerce $ doms :: HashMap (CfEdge ()) [SVal]

      generalCfgFormula ddg cfg
      er <- liftSymbolicT . Q.query $ do
        -- Make sure General formula works
        er' <- Q.checkSat >>= \case
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
    getUndecided :: BranchCond SVal -> Maybe (SVal, UndecidedIfBranches)
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

getUnsatBranches :: Cfg [Stmt] -> IO (Either GeneralSolveError [CfEdge ()])
getUnsatBranches cfg = case checkCfg cfg of
  Left err -> return . Left . TypeCheckerError $ err
  Right (_, cfg', tr) -> do
    let ddg = CfgA.getDataDependenceGraph cfg
    er <- flip (PilSolver.runSolverWith SBV.z3)
          ( PilSolver.emptyState
          , SolverCtx (tr ^. #varSymTypeMap) HashMap.empty False
          )
          $ PilSolver.declarePilVars >> unsatBranches ddg cfg'
    case er of
      Left err -> return . Left $ SolverError tr err
      Right (r, _) -> return $ Right r

_cleanPrunedCfg :: Int -> InterCfg -> InterCfg
_cleanPrunedCfg numItersLeft icfg =
  if icfg == icfg'' || numItersLeft <= 0
    then icfg''
    else -- Recursing until stmts don't change or no iterations left
      _cleanPrunedCfg (numItersLeft - 1) icfg''
 where
  icfg' :: InterCfg
  icfg' = CfgA.copyProp icfg
  -- Need deadNodes to compute removedVars and to actually remove the dead nodes
  deadNodes :: HashSet (CfNode [Stmt])
  deadNodes = CfgA.getDeadNodes (unInterCfg icfg')
  removedVars :: HashSet PilVar
  removedVars = PA.getDefinedVars (concatMap concat deadNodes)
  icfg'' :: InterCfg
  icfg'' =
    CfgA.reducePhi removedVars
      . CfgA.fixed CfgA.removeUnusedPhi
      . CfgA.removeNodes deadNodes
      $ icfg'

cleanPrunedCfg :: InterCfg -> InterCfg
cleanPrunedCfg = removeEmptyBasicBlockNodes' . _cleanPrunedCfg maxIters
 where
  maxIters = 10
  removeEmptyBasicBlockNodes' (InterCfg cfg) = InterCfg . CfgA.removeEmptyBasicBlockNodes $ cfg

simplify_ :: Bool -> Int -> Cfg [Stmt] -> IO (Either GeneralSolveError (Cfg [Stmt]))
simplify_ isRecursiveCall numItersLeft cfg
  | numItersLeft <= 0 = return . Right $ cfg
  | otherwise = do
      let cfg' = unInterCfg . cleanPrunedCfg . InterCfg $ cfg
      if cfg' == cfg && isRecursiveCall
        then return . Right $ cfg'
        else getUnsatBranches cfg' >>= \case
          Left err -> return $ Left err
          Right [] -> return . Right $ cfg'
          Right es -> simplify_ True (numItersLeft - 1)
                      $ foldr Cfg.removeIdEdge cfg' es

simplify :: Cfg [Stmt] -> IO (Either GeneralSolveError (Cfg [Stmt]))
simplify stmts = do
  simplify_ False 10 stmts
