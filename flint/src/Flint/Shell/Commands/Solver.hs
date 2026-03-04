module Flint.Shell.Commands.Solver
  ( solveCommand
  ) where

import Flint.Prelude

import Data.IORef

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import Flint.Analysis.Path.Matcher (asStmts)

import Blaze.Pil.Solver (solveStmtsWithZ3)
import qualified Blaze.Types.Pil.Solver as Solver

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text


solveCommand :: ShellCommand
solveCommand = ShellCommand
  { cmdName = "solve"
  , cmdAliases = ["sv"]
  , cmdHelp = "Solve path(s) with Z3"
  , cmdUsage = "solve <path_id> [path_id ...]"
  , cmdAction = solvePaths
  }

solvePaths :: ShellState -> [Text] -> IO CommandResult
solvePaths _st [] = return $ ResultError "Usage: solve <path_ids>  (e.g. 0 1 2, [0,1,2], [0..5])"
solvePaths st args = do
  let pids = parsePathIds args
  solver <- readIORef (st ^. #useSolver)
  results <- forM pids $ \pid -> do
    mPath <- lookupPath st pid
    case mPath of
      Nothing -> return (pid, "not found")
      Just cp -> do
        let stmts = case cp ^. #pathPrep of
              Just prep -> asStmts $ prep ^. #stmts
              Nothing   -> cp ^. #pilPath
        if not solver
          then return (pid, "Solver disabled. Use 'set solver on' to enable.")
          else do
            result <- catch
              (solveStmtsWithZ3 Solver.AbortOnError stmts)
              (\(e :: SomeException) -> return . Solver.Err . Left . error $ show e)
            return (pid, formatResult result)
  return $ ResultSolver results

formatResult :: Solver.SolverResult -> Text
formatResult = \case
  Solver.Sat solutions
    | HashMap.null solutions -> "SAT (no variable bindings)"
    | otherwise -> "SAT\n" <> Text.unlines
        ((\(k, v) -> "  " <> k <> " = " <> show v) <$> HashMap.toList solutions)
  Solver.Unsat _reasons -> "UNSAT"
  Solver.Unk -> "UNKNOWN"
  Solver.Err _e -> "ERROR (solver error)"
