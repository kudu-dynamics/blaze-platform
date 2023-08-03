module Flint.Analysis
  ( module Flint.Analysis
  ) where

import Flint.Prelude

import Flint.Analysis.Uefi ( resolveCalls )
import Flint.Cfg ( expandCfgToDepth )
import Flint.Types.Analysis
import qualified Flint.Cfg.Store as CfgStore
import Flint.Types.Cfg.Store (CfgStore)

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
import qualified Blaze.Import.Source.BinaryNinja as Binja
import Blaze.Import.Source.BinaryNinja (BNImporter)
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
  paths <- Path.sampleRandomPathsContaining (HashSet.fromList ns) 200 cfg
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

addCfgStoreForBinary
  :: ( CfgImporter imp
     , NodeDataType imp ~ PilNode
     )
  => imp
  -> [Function]
  -> CfgStore
  -> IO CfgStore
addCfgStoreForBinary imp funcs store =
  foldM (\store func -> CfgStore.addFunc imp func store) store funcs

type FlintSearchConfig bin func = [BinarySearchConfig bin func]

data BinarySearchConfig bin func = BinarySearchConfig
  { excludeFuncsFromStore :: HashSet Address
  , searchFuncs :: [FuncSearchConfig func]
  , binary :: bin
  } deriving (Eq, Ord, Show, Generic)

-- TODO: make functions specifiable by address OR symbol name
data QueryDest func bb
  = QueryDestBasicBlock func bb
  | QueryDestFunction func
  deriving (Eq, Ord, Show, Generic)
  
data FuncSearchConfig func = FuncSearchConfig
  { func :: func
  -- , mustReachAll :: [QueryDest func bb]
  -- , mustReachOneOf :: [QueryDest func bb]
  , callExpandDepth :: Word64
  , numSamples :: Word64
  } deriving (Eq, Ord, Show, Generic)

reifyFuncSearchConfig
  :: CallGraphImporter imp
  => imp
  -> FuncSearchConfig Address
  -> IO (FuncSearchConfig Function)
reifyFuncSearchConfig imp fconfig = Cg.getFunction imp (fconfig ^. #func) >>= \case
  Nothing -> error $ "Could not load function at " <> show (fconfig ^. #func)
  Just func -> return $ fconfig & #func .~ func
  
-- | Opens binary specified by FilePath and gets Functions from Addresses
reifyBinarySearchConfig
  :: (BinaryImporter imp, CallGraphImporter imp)
  => BinarySearchConfig FilePath Address
  -> IO (BinarySearchConfig imp Function)
reifyBinarySearchConfig bconfig = (openBinary $ bconfig ^. #binary) >>= \case
  Left err -> error $ cs err
  Right imp -> do
    fconfigs <- traverse (reifyFuncSearchConfig imp) $ bconfig ^. #searchFuncs
    return $ bconfig & #binary .~ imp
                     & #searchFuncs .~ fconfigs

getFuncsForBinarySearchConfig
  :: CallGraphImporter imp
  => BinarySearchConfig imp Function
  -> IO [Function]
getFuncsForBinarySearchConfig bconfig = do
  let imp = bconfig ^. #binary
      excludes = bconfig ^. #excludeFuncsFromStore
  funcs <- Cg.getFunctions imp
  return $ filter (\func -> not $ HashSet.member (func ^. #address) excludes) funcs

storeFromBinarySearchConfig
  :: ( CfgImporter imp
     , CallGraphImporter imp
     , NodeDataType imp ~ PilNode
     )
  => BinarySearchConfig imp Function
  -> CfgStore
  -> IO CfgStore
storeFromBinarySearchConfig bconfig store = do
  let imp = bconfig ^. #binary
      excludes = bconfig ^. #excludeFuncsFromStore
  allFuncs <- Cg.getFunctions imp
  let funcs = filter (\func -> not $ HashSet.member (func ^. #address) excludes) allFuncs
  addCfgStoreForBinary imp funcs store

getPathsForFuncSearchConfig
  :: CfgStore
  -> FuncSearchConfig Function
  -> IO [PilPath]
getPathsForFuncSearchConfig store fconfig = case CfgStore.cfgFromFunc store startFunc of
  Nothing -> error $ "Could not find function in CfgStore:" <> show (startFunc ^. #name)
  Just cfg -> do
    fullCfg <- expandCfgToDepth store (fconfig ^. #callExpandDepth) cfg
    Path.sampleRandomPathsContaining HashSet.empty (fromIntegral $ fconfig ^. #numSamples) fullCfg
  where
    startFunc = fconfig ^. #func
-- summariesOfInterest
--   :: forall imp.
--      ( BinaryImporter imp
--      , CallGraphImporter imp
--      , CfgImporter imp
--      , NodeDataType imp ~ PilNode
--      )
--   => Proxy imp
--   -> [BinarySearchConfig FilePath Address]
--   -> IO ()
-- summariesOfInterest _ bconfigs = do
--   bconfigs' <- traverse reifyBinarySearchConfig bconfigs :: IO [BinarySearchConfig imp Function]
--   cfgStore <- foldM (flip storeFromBinarySearchConfig) CfgStore.init bconfigs'
--   traverse (showSummariesForBinarySearchConfig cfgStore) bconfigs'

  -- where
  --   handleBin :: (BndbFilePath, [(Address, [Address])]) -> IO ()
  --   handleBin (binPath, addrs) = do
  --     putText "\n==================================================="
  --     putText $ show binPath
  --     imp <- getImporter' binPath
  --     handleFuncAddrs imp addrs

  --   handleFuncAddrs :: BNImporter -> [(Address, [Address])] -> IO ()
  --   handleFuncAddrs imp addrs = forM_ addrs $ \(funcAddr, requiredAddrs) -> do
  --     Cg.getFunction imp funcAddr >>= \case
  --       Nothing -> putText $ "Couldn't find function at " <> show funcAddr
  --       Just func -> do
  --         putText "\n||||||||||||||||||||||||||||||||||||||||||||||||||||"
  --         putText $ "Function: " <> func ^. #name
  --         paths <- getOkFunctionPathsContaining imp func requiredAddrs
  --         showPaths "OK Paths" paths


sampleForAllFunctions :: HashSet Text -> BndbFilePath -> IO ()
sampleForAllFunctions blacklist binPath = do
  putText "\n==================================================="
  putText $ show binPath
  imp <- getImporter' binPath
  funcs <- Cg.getFunctions imp
  forM_ funcs $ \func -> do
    putText "\n||||||||||||||||||||||||||||||||||||||||||||||||||||"
    putText $ "Function: " <> func ^. #name
    if HashSet.member (func ^. #name) blacklist
      then putText "Blacklisted function. Causes Flint analysis to fail."
      else do
        paths <- getOkFunctionPathsContaining imp func []
        showPaths "OK Paths" paths
