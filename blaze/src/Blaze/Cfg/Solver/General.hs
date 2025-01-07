{- HLINT ignore "Move mapMaybe" -}

module Blaze.Cfg.Solver.General where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint)

import qualified Blaze.Types.Pil as Pil
import qualified Data.HashSet as HSet
import Blaze.Types.Pil.Checker (DeepSymType, SymTypedStmt)
import Blaze.Types.Cfg (Cfg, CfNode, BranchType(TrueBranch, FalseBranch), CfEdge (CfEdge), PilCfg, PilNode)
import qualified Blaze.Cfg as Cfg
import qualified Blaze.Types.Graph as G
import Blaze.Types.Pil.Analysis ( DataDependenceGraph )
import qualified Blaze.Cfg.Analysis as CfgA
import qualified Blaze.Pil.Solver as PilSolver
import Blaze.Pil.Solver (makeSymVarOfType, logError)
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

-- | Accumulation of branch conditions that should be rechecked during
-- the next iteration ('condsToRecheck') and the edges which were found to be
-- unsat and should be removed ('edgesToRemove').
data BranchCheckAccum = BranchCheckAccum
  { condsToRecheck :: [(UndecidedBranchCond (CfNode [(Int, SymTypedStmt)]), SVal)]
  , edgesToRemove :: [CfEdge (CfNode [(Int, SymTypedStmt)])]
  } deriving (Eq, Show)

-- | If the node contains a conditional branch, and one of the conditional edges
-- has been removed, this returns information about which conditional edge remains
-- and the branching condition.
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
      . HSet.toList
      $ Cfg.succEdges n cfg


-- | Adds overly general constraints for a CFG. Unpruned If conditions are ignored.
-- Phi vars are Or'd together. TODO memory
generalCfgFormula
  :: DataDependenceGraph
  -> Cfg (CfNode [(Int, SymTypedStmt)])
  -> Solver ()
generalCfgFormula ddg cfg = do
  mapM_ addDecidedBranchConstraint decidedBranchConditions
  mapM_ setIndexAndSolveStmt $ Cfg.gatherCfgData cfg
  where
    setIndexAndSolveStmt :: (Int, SymTypedStmt) -> Solver ()
    setIndexAndSolveStmt (i, stmt) = do
      #currentStmtIndex .= i
      solveStmt ddg stmt

    addDecidedBranchConstraint (DecidedBranchCond i x b) = do
      #currentStmtIndex .= i
      r <- bool svNot identity b <$> solveExpr ddg x
      PilSolver.guardBool r
      PilSolver.constrain r

    decidedBranchConditions = mapMaybe (`getDecidedBranchCondNode` cfg)
      . HSet.toList
      . G.nodes
      $ cfg

solveStmt
  :: DataDependenceGraph
  -> SymTypedStmt
  -> Solver ()
solveStmt ddg stmt@(Pil.Stmt _ statement) = case statement of
  -- this is handled elsewhere (only used if there is single outgoing branch edge)
  Pil.BranchCond _ -> return ()
  -- TODO: convert stores/loads into immutable SSA vars
  Pil.Store _ -> return ()
  Pil.DefPhi (Pil.DefPhiOp dest vars) -> do
    unless (any isDependentOn vars) pilSolveStmt
    where
      destDescendants = G.getStrictDescendants dest ddg
      isDependentOn = flip HSet.member destDescendants
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
        logError $ StmtError si e
        fallbackAsFreeVar

----------------------------

data UndecidedBranchCond n = UndecidedBranchCond
  { conditionStatementIndex :: Int
  , condition :: Ch.InfoExpression (Ch.SymInfo, Maybe DeepSymType)
  , trueEdge :: CfEdge n
  , falseEdge :: CfEdge n
  } deriving (Eq, Ord, Show, Generic)

-- | If the node contains a conditional branch, and neither of the conditional edgesToRemove
-- have been removed, this returns an 'UndecidedBranchCond'. Otherwise, 'Nothing' is returned.
getUndecidedBranchCondNode
  :: CfNode [(Int, SymTypedStmt)]
  -> Cfg (CfNode [(Int, SymTypedStmt)])
  -> Maybe (UndecidedBranchCond (CfNode [(Int, SymTypedStmt)]))
getUndecidedBranchCondNode n cfg = case outBranches of
  [(TrueBranch, tnode), (FalseBranch, fnode)] ->
    mcond <*> pure tnode <*> pure fnode
  [(FalseBranch, fnode), (TrueBranch, tnode)] ->
    mcond <*> pure tnode <*> pure fnode
  _ -> Nothing
  where
    mcond = lastMay (Cfg.getNodeData n) >>= \case
      (i, Pil.Stmt _ (Pil.BranchCond (Pil.BranchCondOp x))) -> Just $ UndecidedBranchCond i x
      _ -> Nothing

    outBranches :: [(BranchType, CfEdge (CfNode [(Int, SymTypedStmt)]))]
    outBranches =
      fmap (\e -> (e ^. #branchType, e))
      . HSet.toList
      $ Cfg.succEdges n cfg

-- | Checks individual true and false branches to find impossible constraints.
-- Starts at root node and finds nodes with conditional branches in breadth-first order.
-- Returns a list of inconsistent branch edges.
unsatBranches
  :: DataDependenceGraph
  -> Cfg (CfNode [(Int, SymTypedStmt)])
  -> Solver [CfEdge (CfNode [(Int, SymTypedStmt)])]
unsatBranches ddg typedCfg = case undecidedBranchCondNodes of
  [] -> return []
  ubranches -> do
    generalCfgFormula ddg typedCfg
    ubranchesWithSvals <- traverse getBranchCondSVal ubranches
    er <- liftSymbolicT . Q.query $ Q.checkSat >>= \case
      Q.DSat _ -> Right <$> findRemoveableUnsats ubranchesWithSvals
      Q.Sat -> Right <$> findRemoveableUnsats ubranchesWithSvals
      r -> Left <$> toSolverResult r
    case er of
      Left sr -> do
        putText $ "General constraints are not sat: " <> show sr
        return []
      Right xs -> return xs
  where
    getBranchCondSVal ::
      UndecidedBranchCond (CfNode [(Int, SymTypedStmt)]) ->
      Solver (UndecidedBranchCond (CfNode [(Int, SymTypedStmt)]), SVal)
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

    -- | Returns either generic non-sat result, or list of edges that can be removed.
    --
    -- The general algorithm for `findRemovableUnsats` can be described in imperative
    -- terms:
    -- findRemoveableUnsats(xs) =
    --   for each undecided branch:
    --     if either side is UNSAT:
    --       collect the other side in the general constraints
    --       remove the correct edge
    --     else:
    --       remember to revisit this branch on the next recursive call
    --   if there were any edges removed:
    --     recur into findRemoveableUnsats(branches to revisit)
    --     append that result onto edges to be removed
    --   return edges that should be removed
    findRemoveableUnsats
      :: [(UndecidedBranchCond (CfNode [(Int, SymTypedStmt)]), SVal)]
      -> Query [CfEdge (CfNode [(Int, SymTypedStmt)])]
    findRemoveableUnsats xs = do
      (BranchCheckAccum condsToRecheck edgesToRemove) <-
        foldM checkBranchCond (BranchCheckAccum [] []) xs
      case null edgesToRemove of
        True -> return []
        False -> do
          edgesToRemove' <- findRemoveableUnsats condsToRecheck
          return $ edgesToRemove <> edgesToRemove'
      where
        -- | Check an individual branch condition. If a conditional edge is unsat,
        -- that edge is added to `edgesToRemove` and the constraint implied by the
        -- remaining edge is added to the assertion stack.
        -- If both conditional edges for a branch condition are sat, then the branch
        -- condition is added to the `condsToRecheck` for rechecking later.
        checkBranchCond ::
          BranchCheckAccum ->
          (UndecidedBranchCond (CfNode [(Int, SymTypedStmt)]), SVal) ->
          Query BranchCheckAccum
        checkBranchCond (BranchCheckAccum condsToRecheck edgesToRemove) (u, c) = do
          tryConstraint c >>= \case
            -- True branch is unsat
            False -> do
              -- Add false branch consraint to general formula
              addConstraint $ PilSolver.svBoolNot c

              -- Add true-branch edge to removal list
              return $ BranchCheckAccum condsToRecheck (u ^. #trueEdge : edgesToRemove)

            True -> tryConstraint (PilSolver.svBoolNot c) >>= \case
              -- False branch is unsat
              False -> do
                -- Add true-branch constraint to general formula
                addConstraint c

                -- Add false-branch edge to removal list
                return $ BranchCheckAccum condsToRecheck (u ^. #falseEdge : edgesToRemove)
              True -> -- Both true and false branch are Sat
                return $ BranchCheckAccum ((u, c) : condsToRecheck) edgesToRemove

    addConstraint :: SVal -> Query ()
    addConstraint c = do
      SBV.constrain $ PilSolver.toSBool' c
      Q.push 1

    tryConstraint :: SVal -> Query Bool
    tryConstraint c = Q.inNewAssertionStack
      $ SBV.constrain (PilSolver.toSBool' c) >> isSat

    undecidedBranchCondNodes = mapMaybe (`getUndecidedBranchCondNode` typedCfg)
      . concat
      . G.bfs [Cfg.getRootNode typedCfg]
      $ typedCfg

data GeneralSolveError = TypeCheckerError Ch.ConstraintGenError
                       | SolverError Ch.TypeReport PilSolver.SolverError
                       deriving (Eq, Ord, Show, Generic)

getUnsatBranches ::
  PilCfg ->
  IO (Either GeneralSolveError [CfEdge PilNode])
getUnsatBranches cfg = case checkCfg cfg of
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
      Right (unsatEdges, _) -> return . Right $ fmap getOrigEdge unsatEdges
   where
     getOrigEdge :: CfEdge (CfNode [(Int, SymTypedStmt)]) -> CfEdge PilNode
     getOrigEdge (CfEdge src dst label) =
       let src' = fromJust $ Cfg.getNode cfg (G.getNodeId src)
           dst' = fromJust $ Cfg.getNode cfg (G.getNodeId dst) in
             CfEdge src' dst' label

simplify_ :: Bool -> Int -> PilCfg -> IO (Either GeneralSolveError PilCfg)
simplify_ isRecursiveCall numItersLeft cfg
  | numItersLeft <= 0 = return . Right $ cfg
  | otherwise = do
      let cfg' = CfgA.simplify cfg
      if cfg' == cfg && isRecursiveCall
        then return . Right $ cfg'
        else getUnsatBranches cfg' >>= \case
          Left err -> return $ Left err
          Right [] -> return . Right $ cfg'
          Right es -> simplify_ True (numItersLeft - 1)
                      $ foldr Cfg.removeEdge cfg' es

simplify :: PilCfg -> IO (Either GeneralSolveError PilCfg)
simplify stmts = do
  simplify_ False 10 stmts
