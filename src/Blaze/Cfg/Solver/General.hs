module Blaze.Cfg.Solver.General where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)

import Blaze.Types.Pil ( Statement, Stmt )
import qualified Blaze.Types.Pil as Pil
import qualified Data.HashSet as HashSet
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
import Data.SBV.Dynamic (SVal, svNot)
import qualified Data.SBV.Trans.Control as Q
import qualified Data.SBV.Trans as SBV
import qualified Data.HashMap.Strict as HashMap

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
solveExpr ddg expr@(Ch.InfoExpression (Ch.SymInfo _sz xsym, mdst) op) = 
  catchFallbackAndWarn $ case op of
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

data UndecidedBranchCond = UndecidedBranchCond
  { conditionStatementIndex :: Int
  , condition :: Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)
  , trueEdge :: CfEdge ()
  , falseEdge :: CfEdge ()
  } deriving (Eq, Ord, Show, Generic)

-- | If the node is a conditional if-node, and one of the branches has been removed,
-- this returns which branch remains (True or False) and the conditional expr.
getUndecidedBranchCondNode
  :: CfNode [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))]
  -> Cfg [(Int, Statement (Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)))]
  -> Maybe  UndecidedBranchCond
getUndecidedBranchCondNode n cfg = case outBranches of
  [(TrueBranch, tnode), (FalseBranch, fnode)] ->
    mcond <*> pure tnode <*> pure fnode
  [(FalseBranch, fnode), (TrueBranch, tnode)] ->
    mcond <*> pure tnode <*> pure fnode
  _ -> Nothing
  where
    mcond = lastMay (Cfg.getNodeData n) >>= \case
      (i, Pil.BranchCond (Pil.BranchCondOp x)) -> Just $ UndecidedBranchCond i x
      _ -> Nothing
      
    outBranches :: [(BranchType, CfEdge ())]
    outBranches =
      fmap (\e -> (e ^. #branchType, Cfg.asIdEdge e))
      . HashSet.toList
      $ Cfg.succEdges n cfg

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
    generalCfgFormula ddg typedCfg
    ubranchesWithSvals <- traverse getBranchCondSVal ubranches
    er <- liftSymbolicT . Q.query $ findRemoveableUnsats ubranchesWithSvals
    case er of
      Left sr -> do
        putText $ "General constraints are not sat: " <> show sr
        return []
      Right xs -> return xs
  where
    getBranchCondSVal :: UndecidedBranchCond -> Solver (UndecidedBranchCond, SVal)
    getBranchCondSVal u = do
      #currentStmtIndex .= u ^. #conditionStatementIndex
      c <- solveExpr ddg $ u ^. #condition
      PilSolver.guardBool c
      return (u, c)

    isSat :: Query Bool
    isSat = Q.checkSat >>= \case
      Q.DSat _ -> return True
      Q.Sat -> return True
      _ -> return False

    -- | Returns either generic non-sat result, or list of edges that can be removed
    findRemoveableUnsats
      :: [(UndecidedBranchCond, SVal)]
      -> Query (Either SolverResult [CfEdge ()])
    findRemoveableUnsats xs = Q.checkSat >>= \case
      Q.DSat _ -> Q.push 1 >> Right <$> f xs [] []
      Q.Sat -> Q.push 1 >> Right <$> f xs [] []
      r -> Left <$> toSolverResult r
      where
        -- | This goes through unseen. If it finds a pruneable branch,
        -- it puts the edge in toRemove and puts prepends `seen` onto `unseen`
        -- and adds the new constraint to the assertion stack.
        f :: [(UndecidedBranchCond, SVal)]  -- unseen
          -> [(UndecidedBranchCond, SVal)]  -- seen
          -> [CfEdge ()]
          -> Query [CfEdge ()]
        f [] _ toRemove = return toRemove
        f ((u, c):unseen) seen toRemove = do
          tryConstraint c >>= \case
            -- True branch is UnSat
            False -> do 
              -- add false branch consraint to general formula
              addConstraint $ PilSolver.svBoolNot c 

              -- recur, add true edge to be removal list
              f (reverse seen <> unseen) [] (u ^. #trueEdge : toRemove)

            True -> tryConstraint (PilSolver.svBoolNot c) >>= \case
              -- False branch is unsat
              False -> do
                addConstraint c -- true branch constraint to general formula
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
