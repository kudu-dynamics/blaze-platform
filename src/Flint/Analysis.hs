module Flint.Analysis where

import Flint.Prelude

import Flint.Analysis.Uefi ( resolveCalls )
import Flint.Types.Analysis

import Blaze.Types.Function (Function)
import Blaze.Import.Pil (PilImporter(getMappedStatements))
import Blaze.Types.Pil (Expression)

import qualified Blaze.Import.CallGraph as Cg
import qualified Blaze.Import.Cfg as ImpCfg
import Blaze.Import.Cfg (CfgImporter)

import qualified Blaze.Cfg as Cfg
import Blaze.Cfg (CfNode)
import qualified Blaze.Cfg.Path as Path
import Blaze.Cfg.Path (Path, PilPath)
import Blaze.Function (Function)
import qualified Blaze.Import.Source.BinaryNinja as Binja
import Blaze.Import.Source.BinaryNinja (BNImporter)
import Blaze.Pil.Analysis.Rewrite (rewriteStmts)
import qualified Blaze.Pil.Summary as Summary

import Blaze.Pretty (NewlinedList(NewlinedList), pp', prettyStmts', pretty')

import Blaze.Types.Pil (Stmt)
import Blaze.Types.Pil.Summary (CodeSummary(CodeSummary))
import qualified Blaze.Pil.Analysis.Path as PA
import Blaze.Cfg.Path.Solver as PathSolver
    ( solvePaths, z3, SolverLeniency(IgnoreErrors), SolvePathsResult )

import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Data.List (nub)


-- getAddrFromNonRelativeConstLoad :: 

-- findNonRelativeConstLoadsInStmt :: Foldable t => t Expression -> [Expression]

-- -- | Get any instructions that have reads from non-relative addresses
-- -- These can be used like function inputs if you can write arbitrarily to
-- -- physical memory.
-- findReadsToNonRelativeConstAddresses :: 

getFuncPathsContainingAddrs
  :: (CfgImporter a, ImpCfg.NodeDataType a ~ Cfg.CfNode [Stmt])
  => a
  -> Function
  -> [Address]
  -> IO [Path (CfNode [Stmt])]
getFuncPathsContainingAddrs importer func addrs = do
  (Just r) <- ImpCfg.getCfg importer func 0
  let cfg = r ^. #result
      ns = concatMap (`Cfg.getNodesContainingAddress` cfg) addrs
      -- paths = Path.getSimplePathsContaining (HashSet.fromList ns) cfg
  paths <- Path.sampleRandomPathsContaining (HashSet.fromList ns) 20 cfg
  return paths

getFunctionPaths
  :: (CfgImporter a, ImpCfg.NodeDataType a ~ Cfg.CfNode [Stmt])
  => a
  -> Function
  -> IO [Path (CfNode [Stmt])]
getFunctionPaths importer func = do
  (Just r) <- ImpCfg.getCfg importer func 0
  let cfg = r ^. #result
      paths = Path.getAllSimplePaths cfg
      -- stmtPaths = Path.toStmts <$> paths
  -- return stmtPaths
  return paths

getOkPathsFromResults :: SolvePathsResult a -> [a]
getOkPathsFromResults r = sats <> unks -- <> cerrs <> serrs
  where
    sats = snd <$> r ^. #satPaths
    unks = r ^. #unkPaths
    _cerrs = snd <$> r ^. #constraintGenErrorPaths
    _serrs = snd <$> r ^. #solverErrorPaths

filterOkPaths :: [Path (CfNode [Stmt])] -> IO [Path (CfNode [Stmt])]
filterOkPaths paths = do
  putText $ "Got " <> show (length paths) <> " paths"
  r <- solvePaths z3 IgnoreErrors (take 20 $ paths)
  let ok = getOkPathsFromResults r
  putText $ "Got " <> show (length ok) <> " OK paths"
  return ok

-- | Gets all functions paths that are not Unsat.
--   This includes ones we couldn't solve due to type errors, etc.
--   Results are the analyzed PIL statements used by the solver.
getOkFunctionPaths
  :: (CfgImporter a, ImpCfg.NodeDataType a ~ Cfg.CfNode [Stmt])
  => a
  -> Function
  -> IO [PilPath]
getOkFunctionPaths importer func = do
  paths <- getFunctionPaths importer func
  filterOkPaths paths

-- | Gets all functions paths that are not Unsat.
--   This includes ones we couldn't solve due to type errors, etc.
--   Results are the analyzed PIL statements used by the solver.
getOkFunctionPathsContaining
  :: (CfgImporter a, ImpCfg.NodeDataType a ~ Cfg.CfNode [Stmt])
  => a
  -> Function
  -> [Address]
  -> IO [PilPath]
getOkFunctionPathsContaining importer func addrs = do
  paths <- getFuncPathsContainingAddrs importer func addrs
  filterOkPaths paths

simplify :: [Stmt] -> [Stmt]
simplify = resolveCalls . PA.aggressiveExpand --  . PA.simplifyVars

getImporter' :: FilePath -> IO BNImporter
getImporter' = Binja.getImporter

showCodeSummary :: CodeSummary -> IO ()
showCodeSummary s = do
  putText $ "InputVars: " <>  Text.intercalate ", " (pretty' <$> s ^. #inputVars)
  putText "InputLoads:"
  pp' $ nub (s ^. #inputLoads)
  putText "Results:"
  pp' $ s ^. #results
  putText "Effects:"
  pp' . NewlinedList . sort $ s ^. #effects
  return ()

showPaths :: Text -> [Path (CfNode [Stmt])] -> IO ()
showPaths title paths = do
  putText $ "\n\n=========================\n" <> title <> "\n===============================\n"
  forM_ paths $ \p -> do
    let ps = Path.toStmts p
        ps' = simplify ps
    putText "---------------------"
    prettyStmts' ps'
    putText "\n"
    putText "Summary:"
    let summary = Summary.removeKilledWrites . Summary.fromStmts . resolveCalls $ ps'
    showCodeSummary summary
    putText "--------------------\n"

showPathsOfInterest :: [(BndbFilePath, [(Address, [Address])])] -> IO ()
showPathsOfInterest = traverse_ handleBin
  where
    handleBin :: (BndbFilePath, [(Address, [Address])]) -> IO ()
    handleBin (binPath, addrs) = do
      putText "\n==================================================="
      putText $ show binPath
      imp <- getImporter' binPath
      handleFuncAddrs imp addrs

    handleFuncAddrs :: BNImporter -> [(Address, [Address])] -> IO ()
    handleFuncAddrs imp addrs = forM_ addrs $ \(funcAddr, requiredAddrs) -> do
      Cg.getFunction imp funcAddr >>= \case
        Nothing -> putText $ "Couldn't find function at " <> show funcAddr
        Just func -> do
          putText "\n||||||||||||||||||||||||||||||||||||||||||||||||||||"
          putText $ "Function: " <> func ^. #name
          paths <- getOkFunctionPathsContaining imp func requiredAddrs
          showPaths "OK Paths" paths
