module Blaze.Cfg.Solver.BranchContext where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)

import Blaze.Types.Pil ( Statement, Stmt )
import qualified Blaze.Types.Pil as Pil
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Blaze.Types.Pil.Checker (DeepSymType)
import Blaze.Types.Cfg (Cfg, CfNode, BranchType(TrueBranch, FalseBranch), CfEdge)
import Blaze.Types.Cfg.Interprocedural (InterCfg(InterCfg), unInterCfg)
import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Types.Graph as G
import Blaze.Types.Pil.Analysis ( DataDependenceGraph )
import qualified Blaze.Cfg.Analysis as CfgA
import qualified Blaze.Pil.Solver as PilSolver
import Blaze.Pil.Solver (makeSymVarOfType, warn)
import Blaze.Types.Pil.Solver
import qualified Blaze.Types.Pil.Checker as Ch
import Blaze.Cfg.Checker (checkCfg)
import Data.SBV.Dynamic (SVal, svNot, svAnd, svTrue)
import qualified Data.SBV.Trans.Control as Q
import qualified Data.SBV.Trans as SBV
import Blaze.Pretty (pretty)


data DecidedBranchCond = DecidedBranchCond
  { conditionStatementIndex :: Int
  , condition :: Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)
  , decidedBranch :: Bool
  } deriving (Eq, Ord, Show, Generic)

-- | If the node is a conditional if-node, and one of the branches has been removed,
-- this returns which branch remains (True or False) and the conditional expr.
getDecidedBranchCondNode
  :: CfNode [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))]
  -> Cfg [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))]
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
  -> Cfg [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))]
  -> Solver ()
generalCfgFormula ddg cfg = do
  mapM_ addDecidedBranchConstraint decidedBranchConditions
  mapM_ setIndexAndSolveStmt $ Cfg.gatherCfgData cfg
  where
    setIndexAndSolveStmt :: (Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType))) -> Solver ()
    setIndexAndSolveStmt (i, stmt) = do
      #currentStmtIndex .= i
      solveStmt ddg stmt

    addDecidedBranchConstraint (DecidedBranchCond i x b) = do
      #currentStmtIndex .= i
      r <- bool svNot identity b <$> solveExpr ddg x
      PilSolver.guardBool r
      PilSolver.constrain r
      
    decidedBranchConditions = mapMaybe (`getDecidedBranchCondNode` cfg)
      . HashSet.toList
      . G.nodes
      $ cfg

solveStmt
  :: DataDependenceGraph
  -> Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType))
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

data BranchCondType = OnlyTrue (CfEdge ())
                    | OnlyFalse (CfEdge ())
                    | Undecided { falseEdge :: CfEdge (), trueEdge :: CfEdge () }
                    deriving (Eq, Ord, Show, Generic)

data BranchCond a = BranchCond
  { conditionStatementIndex :: Int
  , condition :: a
  , conditionType :: BranchCondType
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

-- | If the node is a conditional if-node, and one of the branches has been removed,
-- this returns which branch remains (True or False) and the conditional expr.
getBranchCondNode
  :: CfNode [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))]
  -> Cfg [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))]
  -> Maybe (BranchCond (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))
getBranchCondNode n cfg = case outBranches of
  [(TrueBranch, tnode), (FalseBranch, fnode)] ->
    mcond <*> (pure $ Undecided tnode fnode)
  [(FalseBranch, fnode), (TrueBranch, tnode)] ->
    mcond <*> (pure $ Undecided tnode fnode)
  [(TrueBranch, tnode)] ->
    mcond <*> (pure $ OnlyTrue tnode)
  [(FalseBranch, fnode)] ->
    mcond <*> (pure $ OnlyFalse fnode)
  _ -> Nothing
  where
    mcond = lastMay (Cfg.getNodeData n) >>= \case
      (i, Pil.BranchCond (Pil.BranchCondOp x)) -> Just $ BranchCond i x
      _ -> Nothing
      
    outBranches :: [(BranchType, CfEdge ())]
    outBranches =
      fmap (\e -> (e ^. #branchType, Cfg.asIdEdge e))
      . HashSet.toList
      $ Cfg.succEdges n cfg

getBranchCondNodes
  :: Cfg [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))]
  -> [BranchCond (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType))]
getBranchCondNodes typedCfg = mapMaybe (flip getBranchCondNode typedCfg)
  . HashSet.toList
  . G.nodes
  $ typedCfg


toSolvedBranchCondNode
  :: DataDependenceGraph
  -> BranchCond (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType))
  -> Solver (BranchCond SVal)
toSolvedBranchCondNode ddg bnode = do
  #currentStmtIndex .= bnode ^. #conditionStatementIndex
  traverse (solveExpr ddg) bnode

getSolvedBranchCondNodes
  :: DataDependenceGraph
  -> Cfg [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))]
  -> Solver [BranchCond SVal]
getSolvedBranchCondNodes ddg =
  traverse (toSolvedBranchCondNode ddg) . getBranchCondNodes


data UndecidedBranchCond = UndecidedBranchCond
  { conditionStatementIndex :: Int
  , selfNode :: CfNode ()
  , condition :: Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)
  , trueEdge :: CfEdge ()
  , falseEdge :: CfEdge ()
  } deriving (Eq, Ord, Show, Generic)

-- | If the node is a conditional if-node, and one of the branches has been removed,
-- this returns which branch remains (True or False) and the conditional expr.
getUndecidedBranchCondNode
  :: CfNode [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))]
  -> Cfg [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))]
  -> Maybe UndecidedBranchCond
getUndecidedBranchCondNode n cfg = case outBranches of
  [(TrueBranch, tnode), (FalseBranch, fnode)] ->
    mcond <*> pure tnode <*> pure fnode
  [(FalseBranch, fnode), (TrueBranch, tnode)] ->
    mcond <*> pure tnode <*> pure fnode
  _ -> Nothing
  where
    mcond = lastMay (Cfg.getNodeData n) >>= \case
      (i, Pil.BranchCond (Pil.BranchCondOp x)) -> Just $ UndecidedBranchCond i (Cfg.asIdNode n) x
      _ -> Nothing
      
    outBranches :: [(BranchType, CfEdge ())]
    outBranches =
      fmap (\e -> (e ^. #branchType, Cfg.asIdEdge e))
      . HashSet.toList
      $ Cfg.succEdges n cfg


-- getBranchContextConstraints
--   :: Cfg [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))]
--   -> [UndecidedBranchCond]
--   -> Solver (HashMap (CfNode ()) [(Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType))])
-- getBranchContextConstraints typedCfg ubranches = do
--   nodesAndConditions <- buildHashMap . concatMap <$> mapM getConstraintsForUndecidedBranches ubranches
--   where
--     buildHashMap :: (Hashable k, Eq k) => [(k, v)] -> HashMap k [v]
--     buildHashMap = foldr (\(k, v) m -> HashMap.insertWith (<>) k [v] m) HashMap.empty

--     getConstraintsForUndecidedBranches :: UndecidedBranchCond
--                                        -> Solver [(CfNode (), SVal)]
--     getConstraintsForUndecidedBranches u = trueNodeConstraints <> falseNodeConstraints
--       where
--         -- TODO: Can't make it with expressions because we don't have checker var syms.
--         -- need to use solver monad. need checker var syms for NOT

--         trueNodeConstraints = fmap (\n -> (n, u ^. #condition)) trueUnique
--         falseNodeConstraints = fmap (\n -> (n, mkNot $ u ^. #condition)) falseUnique
--         mkNot x = Ch.InfoExpression (x ^. #info)
--                   (Pil.NOT . Pil.NotOp $ x)
--         getReachable n = HashSet.fromList . G.reachable n . view #graph $ typedCfg
--         falseReachable = getReachable $ u ^. #falseEdge . #dst
--         trueReachable = getReachable $ u ^. #trueEdge . #dst
--         falseUnique = HashSet.toList $ HashSet.difference falseReachable trueReachable
--         trueUnique :: [CfNode ()]
--         trueUnique = HashSet.toList $ HashSet.difference trueReachable falseReachable
        

-- | Returns mapping of nodes to contextual constraints of unpruned if-nodes.
-- Ignores decided/pruned if-nodes because they will be part of full general-formula
getBranchContextConstraints
  :: Cfg [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))]
  -> [UndecidedBranchCond]
  -> HashMap (CfNode ()) [(Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType))]
getBranchContextConstraints typedCfg = buildHashMap . concatMap getConstraintsForUndecidedBranches
  where
    buildHashMap :: (Hashable k, Eq k) => [(k, v)] -> HashMap k [v]
    buildHashMap = foldr (\(k, v) m -> HashMap.insertWith (<>) k [v] m) HashMap.empty

    getConstraintsForUndecidedBranches :: UndecidedBranchCond
                                       -> [(CfNode (), Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType))]
    getConstraintsForUndecidedBranches u = trueNodeConstraints <> falseNodeConstraints
      where
        -- TODO: Can't make it with expressions because we don't have checker var syms.
        -- need to use solver monad

        trueNodeConstraints = fmap (\n -> (n, u ^. #condition)) trueUnique
        falseNodeConstraints = fmap (\n -> (n, mkNot $ u ^. #condition)) falseUnique
        mkNot x = Ch.InfoExpression (x ^. #info)
                  (Pil.NOT . Pil.NotOp $ x)
        getReachable n = HashSet.fromList . G.reachable n . view #graph $ typedCfg

        -- If branch node can reach its parent, then it is looping and is discarded.
        -- TODO: this removes too many nodes.
        --       Ideally, it would return all the nodes uniquely reachable by the branch node
        --       up until it actually loops back.
        getNonLoopingReachable srcNode dstNode =
          let reached = getReachable dstNode in
            bool HashSet.empty reached $ HashSet.member srcNode reached
        falseReachable = getNonLoopingReachable (u ^. #falseEdge . #src) (u ^. #falseEdge . #dst)
        trueReachable = getNonLoopingReachable (u ^. #trueEdge . #src) (u ^. #trueEdge . #dst)
        falseUnique = HashSet.toList $ HashSet.difference falseReachable trueReachable
        trueUnique :: [CfNode ()]
        trueUnique = HashSet.toList $ HashSet.difference trueReachable falseReachable


-- | Returns mapping of nodes to contextual constraints of pruned and unpruned if-nodes.
-- First argument is a function that takes the `condition` and wraps it with a `not`
getBranchContextConstraints'
  :: forall a. (a -> a)
  -> Cfg [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))]
  -> [BranchCond a]
  -> HashMap (CfNode ()) [a]
getBranchContextConstraints' mkNot typedCfg = buildHashMap . concatMap getConstraintsForBranchNode
  where
    buildHashMap :: (Hashable k, Eq k) => [(k, v)] -> HashMap k [v]
    buildHashMap = foldr (\(k, v) m -> HashMap.insertWith (<>) k [v] m) HashMap.empty

    getConstraintsForBranchNode :: BranchCond a
                                -> [(CfNode (), a)]
    getConstraintsForBranchNode bnode = case bnode ^. #conditionType of
      OnlyTrue te -> fmap (, bnode ^. #condition)
                     . HashSet.toList
                     $ getNonLoopingReachable (te ^. #src) (te ^. #dst)
      OnlyFalse fe -> fmap (, mkNot $ bnode ^. #condition)
                      . HashSet.toList
                      $ getNonLoopingReachable (fe ^. #src) (fe ^. #dst)
      Undecided fe te -> trueNodeConstraints <> falseNodeConstraints
        where
          falseReachable = getNonLoopingReachable (fe ^. #src) (fe ^. #dst)
          trueReachable = getNonLoopingReachable (te ^. #src) (te ^. #dst)
          falseUnique = HashSet.toList $ HashSet.difference falseReachable trueReachable
          trueUnique :: [CfNode ()]
          trueUnique = HashSet.toList $ HashSet.difference trueReachable falseReachable

          trueNodeConstraints = fmap (, bnode ^. #condition) trueUnique
          falseNodeConstraints = fmap (, mkNot $ bnode ^. #condition) falseUnique

      where       
        getReachable n = HashSet.fromList . G.reachable n . view #graph $ typedCfg

        -- If branch node can reach its parent, then it is looping and is discarded.
        -- TODO: this removes too many nodes.
        --       Ideally, it would return all the nodes uniquely reachable by the branch node
        --       up until it actually loops back.
        getNonLoopingReachable srcNode dstNode =
          let reached = getReachable dstNode in
            bool HashSet.empty reached $ HashSet.member srcNode reached


data BranchSVals = BranchSVals
  { condition :: SVal
  , context :: SVal
  } deriving (Show, Generic)

-- -- | Checks individual true and false branches to find impossible constraints.
-- -- Starts at root node and finds if nodes in breadth-first order.
-- -- Returns a list of inconsistant branch edges.
-- unsatBranches
--   :: DataDependenceGraph
--   -> Cfg [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))]
--   -> Solver [CfEdge ()]
-- unsatBranches ddg typedCfg = case undecidedBranchCondNodes of
--   [] -> return []
--   ubranches -> do
--     branchContextSvals <- mapM (traverse $ solveExpr ddg) branchContextMap
--     let branchContextSvals' = foldr svAnd svTrue <$> branchContextSvals
--     generalCfgFormula ddg typedCfg
--     ubranchesWithSvals <- traverse (getBranchCondSVal branchContextSvals') ubranches
--     er <- liftSymbolicT . Q.query $ findRemoveableUnsats ubranchesWithSvals
--     case er of
--       Left sr -> do
--         putText $ "General constraints are not sat: " <> show sr
--         return []
--       Right xs -> return xs
--   where
--     branchContextMap :: HashMap (CfNode ()) [(Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType))]
--     branchContextMap = getBranchContextConstraints typedCfg undecidedBranchCondNodes

--     getBranchCondSVal :: HashMap (CfNode ()) SVal
--                       -> UndecidedBranchCond
--                       -> Solver (UndecidedBranchCond, BranchSVals)
--     getBranchCondSVal contextConstraints u = do
--       #currentStmtIndex .= u ^. #conditionStatementIndex
--       c <- solveExpr ddg $ u ^. #condition
--       PilSolver.guardBool c
--       cc <- case HashMap.lookup (u ^. #selfNode) contextConstraints of
--         Nothing -> return svTrue
--         Just cc -> do
--           putText . pretty . fromJust $ HashMap.lookup (u ^. #selfNode) branchContextMap
--           putText . pretty $ u ^. #condition
--           PilSolver.guardBool cc
--           return cc
--       return (u, BranchSVals { condition = c, context = cc })

--     isSat :: Query Bool
--     isSat = Q.checkSat >>= \case
--       Q.DSat _ -> return True
--       Q.Sat -> return True
--       _ -> return False

--     -- | Returns either generic non-sat result, or list of edges that can be removed
--     findRemoveableUnsats
--       :: [(UndecidedBranchCond, BranchSVals)]
--       -> Query (Either SolverResult [CfEdge ()])
--     findRemoveableUnsats xs = Q.checkSat >>= \case
--       Q.DSat _ -> Q.push 1 >> Right <$> f xs [] []
--       Q.Sat -> Q.push 1 >> Right <$> f xs [] []
--       r -> Left <$> toSolverResult r
--       where
--         -- | This goes through unseen. If it finds a pruneable branch,
--         -- it puts the edge in toRemove and puts prepends `seen` onto `unseen`
--         -- and adds the new constraint to the assertion stack.
--         f :: [(UndecidedBranchCond, BranchSVals)]  -- unseen
--           -> [(UndecidedBranchCond, BranchSVals)]  -- seen
--           -> [CfEdge ()]
--           -> Query [CfEdge ()]
--         f [] _ toRemove = return toRemove
--         f ((u, c):unseen) seen toRemove = do
--           tryConstraint ((c ^. #condition) `svAnd` (c ^. #context)) >>= \case
--             -- True branch is UnSat
--             False -> do 
--               -- add false branch consraint to general formula
--               -- addConstraint . PilSolver.svBoolNot $ c ^. #condition 

--               -- recur, add true edge to be removal list
--               f (reverse seen <> unseen) [] (u ^. #trueEdge : toRemove)

--             True -> tryConstraint (PilSolver.svBoolNot (c ^. #condition) `svAnd` (c ^. #context)) >>= \case
--               -- False branch is unsat
--               False -> do
--                 -- addConstraint $ c ^. #condition -- true branch constraint to general formula
--                 f (reverse seen <> unseen) [] (u ^. #falseEdge : toRemove)
--               True -> -- Both true and false branch are Sat
--                 f unseen ((u, c) : seen) toRemove

--     addConstraint :: SVal -> Query ()
--     addConstraint c = do
--       SBV.constrain $ PilSolver.toSBool' c
--       Q.push 1

--     tryConstraint :: SVal -> Query Bool
--     tryConstraint c = Q.inNewAssertionStack
--       $ SBV.constrain (PilSolver.toSBool' c) >> isSat

--     undecidedBranchCondNodes = mapMaybe (`getUndecidedBranchCondNode` typedCfg)
--       . concat
--       . G.bfs [typedCfg ^. #root]
--       $ typedCfg


-- | Checks individual true and false branches to find impossible constraints.
-- Starts at root node and finds if nodes in breadth-first order.
-- Returns a list of inconsistant branch edges.
unsatBranches
  :: DataDependenceGraph
  -> Cfg [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))]
  -> Solver [CfEdge ()]
unsatBranches ddg typedCfg = case undecidedBranchCondNodes of
  [] -> return []
  ubranches -> do
    branchContextSvals <- mapM (traverse $ solveExpr ddg) branchContextMap
    let branchContextSvals' = foldr svAnd svTrue <$> branchContextSvals
    generalCfgFormula ddg typedCfg
    ubranchesWithSvals <- traverse (getBranchCondSVal branchContextSvals') ubranches
    er <- liftSymbolicT . Q.query $ findRemoveableUnsats ubranchesWithSvals
    case er of
      Left sr -> do
        putText $ "General constraints are not sat: " <> show sr
        return []
      Right xs -> return xs
  where
    branchContextMap :: HashMap (CfNode ()) [(Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType))]
    branchContextMap = getBranchContextConstraints typedCfg undecidedBranchCondNodes

    getBranchCondSVal :: HashMap (CfNode ()) SVal
                      -> UndecidedBranchCond
                      -> Solver (UndecidedBranchCond, BranchSVals)
    getBranchCondSVal contextConstraints u = do
      #currentStmtIndex .= u ^. #conditionStatementIndex
      c <- solveExpr ddg $ u ^. #condition
      PilSolver.guardBool c
      cc <- case HashMap.lookup (u ^. #selfNode) contextConstraints of
        Nothing -> return svTrue
        Just cc -> do
          putText . pretty . fromJust $ HashMap.lookup (u ^. #selfNode) branchContextMap
          putText . pretty $ u ^. #condition
          PilSolver.guardBool cc
          return cc
      return (u, BranchSVals { condition = c, context = cc })

    isSat :: Query Bool
    isSat = Q.checkSat >>= \case
      Q.DSat _ -> return True
      Q.Sat -> return True
      _ -> return False

    -- | Returns either generic non-sat result, or list of edges that can be removed
    findRemoveableUnsats
      :: [(UndecidedBranchCond, BranchSVals)]
      -> Query (Either SolverResult [CfEdge ()])
    findRemoveableUnsats xs = Q.checkSat >>= \case
      Q.DSat _ -> Q.push 1 >> Right <$> f xs [] []
      Q.Sat -> Q.push 1 >> Right <$> f xs [] []
      r -> Left <$> toSolverResult r
      where
        -- | This goes through unseen. If it finds a pruneable branch,
        -- it puts the edge in toRemove and puts prepends `seen` onto `unseen`
        -- and adds the new constraint to the assertion stack.
        f :: [(UndecidedBranchCond, BranchSVals)]  -- unseen
          -> [(UndecidedBranchCond, BranchSVals)]  -- seen
          -> [CfEdge ()]
          -> Query [CfEdge ()]
        f [] _ toRemove = return toRemove
        f ((u, c):unseen) seen toRemove = do
          tryConstraint ((c ^. #condition) `svAnd` (c ^. #context)) >>= \case
            -- True branch is UnSat
            False -> do 
              -- add false branch consraint to general formula
              -- addConstraint . PilSolver.svBoolNot $ c ^. #condition 

              -- recur, add true edge to be removal list
              f (reverse seen <> unseen) [] (u ^. #trueEdge : toRemove)

            True -> tryConstraint (PilSolver.svBoolNot (c ^. #condition) `svAnd` (c ^. #context)) >>= \case
              -- False branch is unsat
              False -> do
                -- addConstraint $ c ^. #condition -- true branch constraint to general formula
                f (reverse seen <> unseen) [] (u ^. #falseEdge : toRemove)
              True -> -- Both true and false branch are Sat
                f unseen ((u, c) : seen) toRemove

    addConstraint :: SVal -> Query ()
    addConstraint c = do
      SBV.constrain $ PilSolver.toSBool' c
      Q.push 1

    tryConstraint :: SVal -> Query Bool
    tryConstraint c = Q.inNewAssertionStack
      $ SBV.constrain (PilSolver.toSBool' c) >> isSat

    undecidedBranchCondNodes = mapMaybe (`getUndecidedBranchCondNode` typedCfg)
      . concat
      . G.bfs [typedCfg ^. #root]
      $ typedCfg



data GeneralSolveError = TypeCheckerError Ch.ConstraintGenError
                       | SolverError Ch.TypeReport PilSolver.SolverError
                       deriving (Eq, Ord, Show, Generic)

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

simplify_ :: Bool -> Int -> Cfg [Stmt] -> IO (Either GeneralSolveError (Cfg [Stmt]))
simplify_ isRecursiveCall numItersLeft cfg
  | numItersLeft <= 0 = return . Right $ cfg
  | otherwise = do
      let cfg' = unInterCfg . CfgA.simplify . InterCfg $ cfg
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
