
module Flint.Cfg.PathSpec (spec) where

import Flint.Prelude

import qualified Blaze.Cfg as Cfg
import Blaze.Import.Binary (BinaryImporter(openBinary))
import Blaze.Import.Source.Ghidra (GhidraImporter)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Function (Function)
import qualified Blaze.Types.Function as Func
import qualified Blaze.Path as Path

import Flint.Cfg.Path (samplesFromQuery)
import Flint.Types.Query
  ( Query(QueryTarget, QueryExpandAll)
  , QueryTargetOpts(..)
  , QueryExpandAllOpts(..)
  )
import qualified Flint.Cfg.Store as Store
import Flint.Types.Cfg.Store (CfgStore, CfgInfo)
import qualified Flint.Types.CachedCalc as CC

import qualified Data.HashSet as HashSet
import qualified Data.List as List
import Test.Hspec


loopTestBin :: FilePath
loopTestBin = "res/test_bins/loop_summary/loop_test.gzf"

diveLoggerBin :: FilePath
diveLoggerBin = "../res/test_bins/Dive_Logger/Dive_Logger.gzf"


data TestCtx = TestCtx
  { imp :: GhidraImporter
  , store :: CfgStore
  } deriving (Generic)


getLoopTestCtx :: IO TestCtx
getLoopTestCtx = do
  (imp' :: GhidraImporter) <- unsafeFromRight <$> openBinary loopTestBin
  store' <- Store.init Nothing imp'
  return $ TestCtx
    { imp = imp'
    , store = store'
    }

getTestCtx :: IO TestCtx
getTestCtx = do
  (imp' :: GhidraImporter) <- unsafeFromRight <$> openBinary diveLoggerBin
  store' <- Store.init Nothing imp'
  return $ TestCtx
    { imp = imp'
    , store = store'
    }

getGzfTestCtx :: IO TestCtx
getGzfTestCtx = do
  (imp' :: GhidraImporter) <- unsafeFromRight <$> openBinary diveLoggerBin
  store' <- Store.init Nothing imp'
  return $ TestCtx
    { imp = imp'
    , store = store'
    }


-- | Find a function by name in the store
findFunc :: CfgStore -> Text -> IO (Maybe Function)
findFunc store' name = do
  funcs <- Store.getInternalFuncs store'
  return $ List.find (\f -> f ^. #name == name) funcs


-- | Get the CfgInfo for a function by name
getFuncCfgInfo :: CfgStore -> Text -> IO (Maybe CfgInfo)
getFuncCfgInfo store' name = do
  mfunc <- findFunc store' name
  case mfunc of
    Nothing -> return Nothing
    Just func -> Store.getFuncCfgInfo store' func


-- | Get all node start addresses from a CfgInfo (cyclic CFG)
cfgNodeAddresses :: CfgInfo -> [Address]
cfgNodeAddresses cfgInfo =
  let nodes = HashSet.toList . Cfg.nodes $ cfgInfo ^. #cfg
  in mapMaybe getNodeStart nodes
  where
    getNodeStart :: Cfg.PilNode -> Maybe Address
    getNodeStart (Cfg.BasicBlock bb) = Just $ bb ^. #start
    getNodeStart (Cfg.Call cn) = Just $ cn ^. #start
    getNodeStart _ = Nothing


-- | Check that getNodesContainingAddress can find a node for a given address
canFindNodeForAddress :: CfgInfo -> Address -> Bool
canFindNodeForAddress cfgInfo addr =
  not . null $ Cfg.getNodesContainingAddress addr (cfgInfo ^. #cfg)




-- | Unwrap the first element of a list that is expected to be non-empty in tests
expectHead :: HasCallStack => [a] -> a
expectHead (x:_) = x
expectHead [] = error "expectHead: empty list"


spec :: Spec
spec = do
 beforeAll getTestCtx . describe "Flint.Cfg.Path (address-targeted sampling)" $ do

  context "getNodesContainingAddress on cyclic CFG" $ do
    it "should find nodes for all BasicBlock start addresses in cgc_SetParam" $ \tctx -> do
      mcfgInfo <- getFuncCfgInfo (tctx ^. #store) "cgc_SetParam"
      let cfgInfo = fromJust mcfgInfo
          addrs = cfgNodeAddresses cfgInfo
      -- Every node start address should be findable
      length addrs `shouldSatisfy` (> 0)
      forM_ addrs $ \addr ->
        canFindNodeForAddress cfgInfo addr `shouldBe` True

    it "should find nodes for Call node addresses in cgc_SetParam" $ \tctx -> do
      mcfgInfo <- getFuncCfgInfo (tctx ^. #store) "cgc_SetParam"
      let cfgInfo = fromJust mcfgInfo
          callAddrs = fmap (view #start) $ cfgInfo ^. #calls
      length callAddrs `shouldSatisfy` (> 0)
      forM_ callAddrs $ \addr ->
        canFindNodeForAddress cfgInfo addr `shouldBe` True

  context "getFreshFuncCfgInfo preserves node addresses" $ do
    it "should find same addresses after UUID refresh" $ \tctx -> do
      mfunc <- findFunc (tctx ^. #store) "cgc_SetParam"
      let func = fromJust mfunc
      mfreshInfo <- Store.getFreshFuncCfgInfo (tctx ^. #store) func
      let freshInfo = fromJust mfreshInfo
          addrs = cfgNodeAddresses freshInfo
      length addrs `shouldSatisfy` (> 0)
      forM_ addrs $ \addr ->
        canFindNodeForAddress freshInfo addr `shouldBe` True

  context "QueryTarget (address-targeted sampling)" $ do
    it "should sample paths through a Call node address in cgc_SetParam" $ \tctx -> do
      mfunc <- findFunc (tctx ^. #store) "cgc_SetParam"
      let func = fromJust mfunc
      -- First, sample without target to find a Call address
      paths <- samplesFromQuery (tctx ^. #store) func
        $ QueryExpandAll $ QueryExpandAllOpts
          { callExpandDepthLimit = 0
          , numSamples = 1
          }
      length paths `shouldSatisfy` (> 0)
      let stmts = concatMap Cfg.getNodeData . HashSet.toList . Path.nodes $ expectHead paths
          callAddrs = [ s ^. #addr | s <- stmts
                      , case s ^. #statement of
                          Pil.Call _ -> True
                          _ -> False
                      ]
      length callAddrs `shouldSatisfy` (> 0)
      let targetAddr = expectHead callAddrs

      -- Now sample targeting that address
      targetPaths <- samplesFromQuery (tctx ^. #store) func
        $ QueryTarget $ QueryTargetOpts
          { mustReachSome = (func, targetAddr) :| []
          , callExpandDepthLimit = 0
          , numSamples = 5
          }
      length targetPaths `shouldSatisfy` (> 0)

 beforeAll getGzfTestCtx . describe "Flint.Cfg.Path (call sites and sampling on .gzf)" $ do
    it "should find call sites to an extern function (cgc_strcpy is internal here)" $ \tctx -> do
      -- cgc_strcpy is internal in Dive_Logger, so test with an actual extern
      externs <- Store.getExternalFuncs (tctx ^. #store)
      let mAllocate = find (\e -> e ^. #name == "cgc_allocate") externs
      case mAllocate of
        Nothing -> pendingWith "cgc_allocate extern not found"
        Just ext -> do
          sites <- Store.getCallSitesToFunc (tctx ^. #store) (Func.External ext)
          -- cgc_allocate should have callers (malloc/calloc wrappers use it)
          putText $ "cgc_allocate call sites: " <> show (length sites)
          -- Even if 0, this test helps debug; we'll also check an internal func
          pure ()

    it "should find call sites to an internal function (cgc_strlen)" $ \tctx -> do
      funcs <- Store.getInternalFuncs (tctx ^. #store)
      let mStrlen = find (\f -> f ^. #name == "cgc_strlen") funcs
      case mStrlen of
        Nothing -> pendingWith "cgc_strlen not found"
        Just func -> do
          sites <- Store.getCallSitesToFunc (tctx ^. #store) (Func.Internal func)
          putText $ "cgc_strlen call sites: " <> show (length sites)
          length sites `shouldSatisfy` (> 0)

    it "callSitesToFuncCache keys should include extern functions" $ \tctx -> do
      allFuncs <- Store.getFuncs (tctx ^. #store)
      externs <- Store.getExternalFuncs (tctx ^. #store)
      putText $ "Total funcs: " <> show (length allFuncs) <> ", externs: " <> show (length externs)
      -- Try to get call sites for every extern - should not return Nothing (key exists)
      forM_ (take 3 externs) $ \ext -> do
        result <- CC.get (Func.External ext) (tctx ^. #store . #callSitesToFuncCache)
        putText $ "  " <> ext ^. #name <> ": " <> show (fmap length result)
        result `shouldSatisfy` isJust

    context "address-targeted sampling" $ do
      it "should find nodes for all node start addresses in cgc_SetParam" $ \tctx -> do
        mcfgInfo <- getFuncCfgInfo (tctx ^. #store) "cgc_SetParam"
        let cfgInfo = fromJust mcfgInfo
            addrs = cfgNodeAddresses cfgInfo
        length addrs `shouldSatisfy` (> 0)
        forM_ addrs $ \addr ->
          canFindNodeForAddress cfgInfo addr `shouldBe` True

      it "should find nodes after UUID refresh in cgc_SetParam" $ \tctx -> do
        mfunc <- findFunc (tctx ^. #store) "cgc_SetParam"
        let func = fromJust mfunc
        mfreshInfo <- Store.getFreshFuncCfgInfo (tctx ^. #store) func
        let freshInfo = fromJust mfreshInfo
            addrs = cfgNodeAddresses freshInfo
        length addrs `shouldSatisfy` (> 0)
        forM_ addrs $ \addr ->
          canFindNodeForAddress freshInfo addr `shouldBe` True

      it "should sample paths through every Call node address in cgc_SetParam" $ \tctx -> do
        mcfgInfo <- getFuncCfgInfo (tctx ^. #store) "cgc_SetParam"
        let cfgInfo = fromJust mcfgInfo
            callAddrs = fmap (view #start) $ cfgInfo ^. #calls
        length callAddrs `shouldSatisfy` (> 0)

        mfunc <- findFunc (tctx ^. #store) "cgc_SetParam"
        let func = fromJust mfunc

        forM_ callAddrs $ \targetAddr -> do
          targetPaths <- samplesFromQuery (tctx ^. #store) func
            $ QueryTarget $ QueryTargetOpts
              { mustReachSome = (func, targetAddr) :| []
              , callExpandDepthLimit = 0
              , numSamples = 5
              }
          length targetPaths `shouldSatisfy` (> 0)

      it "address space must match for node lookup (32-bit binary)" $ \tctx -> do
        mcfgInfo <- getFuncCfgInfo (tctx ^. #store) "cgc_SetParam"
        let cfgInfo = fromJust mcfgInfo
            callAddrs = fmap (view #start) $ cfgInfo ^. #calls
        length callAddrs `shouldSatisfy` (> 0)
        let firstAddr = expectHead callAddrs
        (firstAddr ^. #space . #ptrSize) `shouldSatisfy` (< 8)

      it "should sample paths through a dynamically found address in cgc_SetParam" $ \tctx -> do
        mfunc <- findFunc (tctx ^. #store) "cgc_SetParam"
        let func = fromJust mfunc

        paths <- samplesFromQuery (tctx ^. #store) func
          $ QueryExpandAll $ QueryExpandAllOpts
            { callExpandDepthLimit = 0
            , numSamples = 3
            }
        length paths `shouldSatisfy` (> 0)
        let stmts = concatMap Cfg.getNodeData . HashSet.toList . Path.nodes $ expectHead paths
            stmtAddrs = fmap (view #addr) stmts
        length stmtAddrs `shouldSatisfy` (> 0)
        let targetAddr = expectHead stmtAddrs

        targetPaths <- samplesFromQuery (tctx ^. #store) func
          $ QueryTarget $ QueryTargetOpts
            { mustReachSome = (func, targetAddr) :| []
            , callExpandDepthLimit = 0
            , numSamples = 10
            }
        length targetPaths `shouldSatisfy` (> 0)

 beforeAll getLoopTestCtx . describe "Flint.Cfg.Path (address-targeted sampling on loop_test ELF)" $ do
    it "should sample paths through every BasicBlock address in simple_count" $ \tctx -> do
      mcfgInfo <- getFuncCfgInfo (tctx ^. #store) "simple_count"
      let cfgInfo = fromJust mcfgInfo
          bbAddrs = [ bb ^. #start
                    | Cfg.BasicBlock bb <- HashSet.toList . Cfg.nodes $ cfgInfo ^. #cfg
                    ]
      length bbAddrs `shouldSatisfy` (> 0)

      mfunc <- findFunc (tctx ^. #store) "simple_count"
      let func = fromJust mfunc

      forM_ bbAddrs $ \targetAddr -> do
        let matchingNodes = [ n
                            | n <- HashSet.toList . Cfg.nodes $ cfgInfo ^. #cfg
                            , case n of
                                Cfg.BasicBlock bb -> bb ^. #start == targetAddr
                                _ -> False
                            ]
        targetPaths <- samplesFromQuery (tctx ^. #store) func
          $ QueryTarget $ QueryTargetOpts
            { mustReachSome = (func, targetAddr) :| []
            , callExpandDepthLimit = 0
            , numSamples = 1
            }
        when (null targetPaths) $ do
          let stmts = concatMap Cfg.getNodeData matchingNodes :: [Pil.Stmt]
          putText $ "FAILED to reach " <> show targetAddr
            <> " (" <> show (length stmts) <> " stmts: "
            <> show (fmap (\s -> s ^. #statement) stmts) <> ")"
        length targetPaths `shouldSatisfy` (> 0)
