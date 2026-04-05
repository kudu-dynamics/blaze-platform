module Flint.Analysis
  ( module Flint.Analysis
  ) where

import Flint.Prelude

import Flint.Analysis.Path.Matcher (MatcherState, IsExpression)
import qualified Flint.Analysis.Path.Matcher as Matcher
import Flint.Analysis.Uefi ( resolveCalls )
import qualified Flint.Cfg.Store as CfgStore
import Flint.Types.Cfg.Store (CfgStore)

import Blaze.Types.Function (Function)

import Blaze.Import.CallGraph (CallGraphImporter)
import qualified Blaze.Import.Cfg as ImpCfg
import Blaze.Import.Cfg (CfgImporter)

import qualified Blaze.Cfg as Cfg
import Blaze.Types.Cfg (CfNode)
import qualified Blaze.Cfg.Path as Path
import Blaze.Cfg.Path (Path, PilPath)
import qualified Blaze.Pil.Summary as Summary

import Blaze.Pretty (NewlinedList(NewlinedList), pp', prettyStmts', pretty')

import Blaze.Types.Pil (Stmt)
import Blaze.Types.Pil.Summary (CodeSummary)
import qualified Blaze.Pil.Analysis.Path as PA
import Blaze.Cfg.Path.Solver as PathSolver
    ( solvePaths, z3, SolverLeniency(IgnoreErrors), SolvePathsResult )

import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

import Flint.Types.Query
import Blaze.Types.Import


getFuncPathsContainingAddrs
  :: (CfgImporter a, ImpCfg.NodeDataType a ~ Cfg.CfNode [Stmt])
  => a
  -> Function
  -> [Address]
  -> IO [Path (CfNode [Stmt])]
getFuncPathsContainingAddrs importer func addrs = do
  (Just r) <- ImpCfg.getCfg importer func 0
  let cfg = r ^. #result
      ns = concatMap (`Cfg.getNodesContainingOrNearestAddress` cfg) addrs
      -- paths = Path.getSimplePathsContaining (HashSet.fromList ns) cfg
  Path.sampleRandomPathsContaining (HashSet.fromList ns) 200 cfg

getFuncPathsContainingAddrsWithTypeHints
  :: (CfgImporter a, ImpCfg.NodeDataType a ~ Cfg.CfNode [Stmt])
  => a
  -> Function
  -> [Address]
  -> IO ([Path (CfNode [Stmt])], TypeHints)
getFuncPathsContainingAddrsWithTypeHints importer func addrs = do
  (Just r, typeHints) <- ImpCfg.getCfgWithTypeHints importer func 0
  let cfg = r ^. #result
      ns = concatMap (`Cfg.getNodesContainingOrNearestAddress` cfg) addrs
      -- paths = Path.getSimplePathsContaining (HashSet.fromList ns) cfg
  paths <- Path.sampleRandomPathsContaining (HashSet.fromList ns) 200 cfg
  return (paths, typeHints)

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

getFunctionPathsWithTypeHints
  :: (CfgImporter a, ImpCfg.NodeDataType a ~ Cfg.CfNode [Stmt])
  => a
  -> Function
  -> IO ([Path (CfNode [Stmt])], TypeHints)
getFunctionPathsWithTypeHints importer func = do
  (Just r, typeHints) <- ImpCfg.getCfgWithTypeHints importer func 0
  let cfg = r ^. #result
      paths = Path.getAllSimplePaths cfg
      -- stmtPaths = Path.toStmts <$> paths
  -- return stmtPaths
  return (paths, typeHints)

getOkPathsFromResults :: SolvePathsResult a -> [a]
getOkPathsFromResults r = sats <> unks -- <> cerrs <> serrs
  where
    sats = snd <$> r ^. #satPaths
    unks = r ^. #unkPaths
    _cerrs = snd <$> r ^. #constraintGenErrorPaths
    _serrs = snd <$> r ^. #solverErrorPaths

filterOkPaths :: TypeHints -> [Path (CfNode [Stmt])] -> IO [Path (CfNode [Stmt])]
filterOkPaths typeHints paths = do
  putText $ "Got " <> show (length paths) <> " paths"
  r <- solvePaths z3 IgnoreErrors typeHints paths
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
  (paths, typeHints) <- getFunctionPathsWithTypeHints importer func
  filterOkPaths typeHints paths

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
  (paths, typeHints) <- getFuncPathsContainingAddrsWithTypeHints importer func addrs
  filterOkPaths typeHints paths

simplify :: [Stmt] -> [Stmt]
simplify = resolveCalls . PA.aggressiveExpand --  . PA.simplifyVars

showCodeSummary :: CodeSummary -> IO ()
showCodeSummary s = do
  putText $ "InputVars: " <>  Text.intercalate ", " (pretty' <$> HashSet.toList (s ^. #inputVars))
  putText "InputLoads:"
  pp' . HashSet.toList $ s ^. #inputLoads
  putText "Results:"
  pp' . HashSet.toList $ s ^. #results
  putText "Effects:"
  pp' . NewlinedList . sort $ s ^. #effects
  return ()

showPaths :: Text -> [PilPath] -> IO ()
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

showPathsWithMatches
  :: IsExpression expr
  => Text
  -> [(PilPath, [((MatcherState expr stmt, [stmt]), BugMatch)])]
  -> IO ()
showPathsWithMatches title paths = do
  putText $ "\n\n=========================\n" <> title <> "\n===============================\n"
  forM_ paths $ \(p, matches) -> do
    case matches of
      [] -> putText "no matches"
      _ -> do
        let ps = Path.toStmts p
            ps' = simplify ps
        putText "---------------------"
        prettyStmts' ps'
        putText "\n"
        putText "Summary:"
        let summary = Summary.removeKilledWrites . Summary.fromStmts . resolveCalls $ ps'
        showCodeSummary summary
        putText "--------------------\n"
        putText "Bug Matches:"
        traverse_ showBugMatch matches

showBugMatch
  :: IsExpression expr
  => ((MatcherState expr stmt, [stmt]), BugMatch)
  -> IO ()
showBugMatch ((ms, _stmts), bm) = do
  putText $ bm ^. #bugName <> ":"
  putText "Found Primitive:"
  putText $ resolveText $ bm ^. #bugDescription
  putText "\nSuggested Mitigation:"
  putText $ resolveText $ bm ^. #mitigationAdvice      
  where
    resolveText = Matcher.resolveBoundText (ms ^. #boundSyms)

addCfgStoreForBinary
  :: [Function]
  -> CfgStore
  -> IO ()
addCfgStoreForBinary funcs store = mapM_ (CfgStore.addFunc store) funcs

type FlintSearchConfig bin func = [BinarySearchConfig bin func]


reifyQuery
  :: (GetFunction a, CallGraphImporter imp)
  => imp
  -> Query a
  -> IO (Query Function)
reifyQuery imp = traverse (getFunction imp)

showQueryHeader :: Function -> Query Function -> Text
showQueryHeader startFunc = \case
  QueryTarget opts -> (startFunc ^. #name) <> " ==> " <> showTargets (opts ^. #mustReachSome)
  QueryExpandAll _opts -> (startFunc ^. #name) <> " ========== Expand all "
  QueryExploreDeep _opts -> (startFunc ^. #name) <> " ========== Explore deep "
  QueryAllPaths -> (startFunc ^. #name) <> " ========== All Paths "
  QueryCallSeq _opts -> (startFunc ^. #name) <> " ========== Call Seq " -- TODO: show call seq path
  where
    showTargets :: NonEmpty (Function, Address) -> Text
    showTargets (x :| xs) = Text.intercalate ", " . fmap showTarget $ x:xs
    showTarget :: (Function, Address) -> Text
    showTarget (func, addr) = pretty' addr <> func ^. #name

-- showQuerySummaries :: [TaintPropagator] -> CfgStore -> Function -> Query Function -> [BugMatch] -> IO ()
-- showQuerySummaries tps store startFunc q bugMatchers = do
--   paths <- samplesFromQuery store startFunc q
--   let okPaths = paths -- filterOkPaths paths
--       withMatches = flip fmap okPaths
--         $ \p -> let pathPrep = mkPathPrep tps p in
--                   ( p
--                   , flip fmap bugMatchers $ \bm ->
--                       -- TODO: use IO version `matchPath.
--                       -- But first we need to fix solver (issue #436)
--                       ( pureMatch (bm ^. #pathPattern) pathPrep
--                       , bm
--                       )
--                   )
--   showPathsWithMatches (showQueryHeader startFunc q) withMatches
