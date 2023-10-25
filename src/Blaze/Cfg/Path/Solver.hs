module Blaze.Cfg.Path.Solver
  ( module Blaze.Cfg.Path.Solver
  , module Exports
  ) where

import Blaze.Prelude

import Blaze.Types.Cfg.Path (PilPath)
import qualified Blaze.Types.Cfg.Path as Path
import Blaze.Types.Pil (Stmt)
import qualified Blaze.Pil.Analysis.Path as PathA
import Blaze.Pil.Solver (solveStmtsWith_, solveStmtsWith)
import Blaze.Types.Pil.Checker (ConstraintGenError, TypeReport)
import Blaze.Pil.Solver as Exports (z3, cvc4, SMTConfig, SolverResult(..), SolverError(..), SolverReport(..), CV, SolverLeniency(..))
import Blaze.Pil.Analysis.Rewrite (rewriteStmts)
import Blaze.Pretty (prettyPrint', prettyIndexedStmts')
import qualified Blaze.Types.Pil.Checker as Ch
import Blaze.Util.Analysis (untilFixedPoint)


simplifyForSolving_ :: Int -> [Stmt] -> [Stmt]
simplifyForSolving_ = untilFixedPoint (Just errMsg) $ rewriteStmts . PathA.expandVars . PathA.simplifyVars
  where
    errMsg = "simplifyForSolving_: Max iters reached"

simplifyForSolving :: [Stmt] -> [Stmt]
simplifyForSolving = simplifyForSolving_ 10

-- | Prepares Path for solving by transformative analyses.
prepPath :: PilPath -> [Stmt]
prepPath = simplifyForSolving . Path.toStmts

solvePathWith
  :: SMTConfig
  -> SolverLeniency
  -> PilPath
  -> IO (Either
          (Either
            ConstraintGenError
            (SolverError, TypeReport))
          (SolverReport, TypeReport))
solvePathWith solverCfg leniency = solveStmtsWith solverCfg leniency . prepPath

solvePathWith_ :: SMTConfig -> SolverLeniency -> PilPath -> IO SolverResult
solvePathWith_ solverCfg leniency = solveStmtsWith_ solverCfg leniency . prepPath

data SolvePathsResult a = SolvePathsResult
  { satPaths :: [(HashMap Text CV, a)]
  , unsatPaths :: [(Maybe [Text], a)]
  , unkPaths :: [a]
  , constraintGenErrorPaths :: [(ConstraintGenError, a)]
  , solverErrorPaths :: [((SolverError, Ch.TypeReport), a)]
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  
solvePaths :: SMTConfig -> SolverLeniency -> [PilPath] -> IO (SolvePathsResult PilPath)
solvePaths solverCfg leniency paths = do
  solved <- mapConcurrently (\p -> (,p) <$> solvePathWith_ solverCfg leniency p) paths
  return $ foldr divideSolved (SolvePathsResult [] [] [] [] []) solved
  where
    divideSolved :: (SolverResult, PilPath) -> SolvePathsResult PilPath -> SolvePathsResult PilPath
    divideSolved (sr, p) r = case sr of
      Sat x -> r & #satPaths %~ ((x, p):)
      Unk -> r & #unkPaths %~ (p:)
      Unsat x -> r & #unsatPaths %~ ((fmap cs <$> x, p):)
      Err (Left err) -> r & #constraintGenErrorPaths %~ ((err, p):)
      Err (Right err) -> r & #solverErrorPaths %~ ((err, p):)

toPreppedStmts :: SolvePathsResult PilPath -> SolvePathsResult [(Int, Stmt)]
toPreppedStmts = fmap $ zip [0..] . prepPath

prettyUnsat :: (Maybe [Text], [(Int, Stmt)]) -> IO ()
prettyUnsat (x, p) = do
  prettyPrint' x
  prettyIndexedStmts' p

prettyUnsats :: SolvePathsResult PilPath -> IO ()
prettyUnsats = traverse_ (const (putText "\n") <=< prettyUnsat) . view #unsatPaths . toPreppedStmts

prettySat :: (HashMap Text CV, [(Int, Stmt)]) -> IO ()
prettySat (x, p) = do
  prettyPrint' x
  prettyIndexedStmts' p

prettySats :: SolvePathsResult PilPath -> IO ()
prettySats = traverse_ (const (putText "\n") <=< prettySat) . view #satPaths . toPreppedStmts

prettyUnk :: [(Int, Stmt)] -> IO ()
prettyUnk = prettyIndexedStmts'

prettyUnks :: SolvePathsResult PilPath -> IO ()
prettyUnks = traverse_ (const (putText "\n") <=< prettyUnk) . view #unkPaths . toPreppedStmts

prettyErr :: Either Ch.ConstraintGenError (SolverError, Ch.TypeReport) -> IO ()
prettyErr = \case
  Left err -> pprint err
  Right (serr, _tr) -> pprint serr
