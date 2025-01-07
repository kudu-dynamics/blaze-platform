module Flint.Analysis
  ( module Flint.Analysis
  ) where

import Flint.Prelude

import Flint.Analysis.Path.Matcher ( pureMatch, MkPathPrep(mkPathPrep), MatcherResult, MatcherState )
import qualified Flint.Analysis.Path.Matcher as Matcher
import Flint.Analysis.Uefi ( resolveCalls )
import Flint.Types.Analysis
import qualified Flint.Cfg.Store as CfgStore
import Flint.Types.Cfg.Store (CfgStore)
import Flint.Cfg.Path (samplesFromQuery)

import Blaze.Types.Function (Function)

import Blaze.Import.Binary (BinaryImporter, openBinary)
import Blaze.Import.CallGraph (CallGraphImporter)
import qualified Blaze.Import.CallGraph as Cg
import qualified Blaze.Import.Cfg as ImpCfg
import Blaze.Import.Cfg (CfgImporter, NodeDataType)

import qualified Blaze.Cfg as Cfg
import Blaze.Types.Cfg (CfNode, PilNode)
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
import Data.List (nub)

import Flint.Types.Query


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
  Path.sampleRandomPathsContaining (HashSet.fromList ns) 200 cfg

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
  r <- solvePaths z3 IgnoreErrors paths
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

showPathsWithMatches :: Text -> [(PilPath, [((MatcherState a, MatcherResult), BugMatch)])] -> IO ()
showPathsWithMatches title paths = do
  putText $ "\n\n=========================\n" <> title <> "\n===============================\n"
  forM_ paths $ \(p, matches) -> do
    let matchMatch ((_, Matcher.Match _), _) = True
        matchMatch _ = False
        containsAnyMatches = any matchMatch matches
    case containsAnyMatches of
      False -> putText "no matches"
      True -> do
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

showBugMatch :: ((MatcherState a, MatcherResult), BugMatch) -> IO ()
showBugMatch ((ms, r), bm) = do
  putText $ bm ^. #bugName <> ":"
  case r of
    Matcher.NoMatch -> putText "No match."
    Matcher.Match _stmts -> do
      putText "Found Primitive:"
      putText $ resolveText $ bm ^. #bugDescription
      putText "\nSuggested Mitigation:"
      putText $ resolveText $ bm ^. #mitigationAdvice      
  where
    resolveText = Matcher.resolveBoundText (ms ^. #boundSyms)

addCfgStoreForBinary
  :: ( CfgImporter imp
     , NodeDataType imp ~ PilNode
     )
  => imp
  -> [Function]
  -> CfgStore
  -> IO ()
addCfgStoreForBinary imp funcs store = mapM_ (CfgStore.addFunc imp store) funcs

type FlintSearchConfig bin func = [BinarySearchConfig bin func]


reifyQuery
  :: (GetFunction a, CallGraphImporter imp)
  => imp
  -> Query a
  -> IO (Query Function)
reifyQuery imp = traverse (getFunction imp)

-- TODO: Remove because the CfgStore loads func Cfgs lazily.
storeFromBinarySearchConfig
  :: ( CfgImporter imp
     , CallGraphImporter imp
     , NodeDataType imp ~ PilNode
     )
  => imp
  -> BinarySearchConfig imp Function
  -> CfgStore
  -> IO ()
storeFromBinarySearchConfig imp bconfig store = do
  let excludes = HashSet.fromList $ bconfig ^. #excludeFuncsFromStore
  allFuncs <- Cg.getFunctions imp
  let funcs = filter (\func -> not $ HashSet.member func excludes) allFuncs
  addCfgStoreForBinary imp funcs store

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

showQuerySummaries :: [TaintPropagator] -> CfgStore -> Function -> Query Function -> [BugMatch] -> IO ()
showQuerySummaries tps store startFunc q bugMatchers = do
  paths <- samplesFromQuery store startFunc q
  let okPaths = paths -- filterOkPaths paths
      withMatches = flip fmap okPaths
        $ \p -> let pathPrep = mkPathPrep tps p in
                  ( p
                  , flip fmap bugMatchers $ \bm ->
                      -- TODO: use IO version `matchPath.
                      -- But first we need to fix solver (issue #436)
                      ( pureMatch (bm ^. #pathPattern) pathPrep
                      , bm
                      )
                  )
  showPathsWithMatches (showQueryHeader startFunc q) withMatches

summariesOfInterest
  :: forall imp func.
     ( BinaryImporter imp
     , CallGraphImporter imp
     , CfgImporter imp
     , NodeDataType imp ~ PilNode
     , GetFunction func
     )
  => [TaintPropagator]
  -> BinarySearchConfig imp func
  -> IO ()
summariesOfInterest tps bconfig = do
  imp <- openBinary (bconfig ^. #binaryPath) >>= either (error . cs) return
  bconfig' <- traverse (getFunction imp) bconfig
  cfgStore <- CfgStore.init Nothing imp
  storeFromBinarySearchConfig imp bconfig' cfgStore
  mapM_ (\x -> showQuerySummaries tps cfgStore (x ^. #startFunc) (x ^. #query) (x ^. #bugMatches))
    $ bconfig' ^. #queryConfigs
