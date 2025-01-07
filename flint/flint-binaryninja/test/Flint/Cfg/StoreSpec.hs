module Flint.Cfg.StoreSpec where

import Flint.Prelude

import qualified Flint.Cfg.Store as CfgStore
import Flint.Types.Query (getFunction, FuncConfig(FuncSym, FuncAddr))

import qualified Blaze.Import.CallGraph as CG
import Blaze.Import.Binary (BinaryImporter(openBinary))
import Blaze.Import.Source.BinaryNinja (BNImporter)
import Blaze.Import.Source.Ghidra (GhidraImporter)
import Blaze.Types.Function (Function)
import qualified Blaze.Types.Graph as G
import qualified Blaze.Types.Pil as Pil

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

import Test.Hspec

pltThunkLib :: FilePath
pltThunkLib = "../res/test_bins/plt_thunk/libplt_thunk.so"

shimMainBndb :: FilePath
shimMainBndb = "res/test_bins/shim/shim_main.bndb"

-- Contains strlen and strchr
shimLibCBndb :: FilePath
shimLibCBndb = "res/test_bins/shim/shim_libc.so.bndb"

type SimpFunc = (Text, Address)

spec :: Spec
spec = describe "Flint.Cfg.Store" $ do
  context "PLT Thunks (Binja)" $ do
    (bv :: BNImporter) <- unsafeFromRight <$> runIO (openBinary pltThunkLib)
    funcsWithCfgs <- runIO $ CfgStore.getFuncsWithCfgs bv
    let simpFunc :: Function -> SimpFunc
        simpFunc func = (func ^. #name, func ^. #address)
        pltFuncMapping = CfgStore.getPltThunkMapping funcsWithCfgs
        pltFuncMapping' :: HashMap SimpFunc (SimpFunc, Either SimpFunc Pil.ConstFuncPtrOp)
        pltFuncMapping' = HashMap.fromList
          . fmap (\(func, x) ->
                    ( simpFunc func
                    , ( simpFunc $ x ^. #renamedFunc
                      , case x ^. #newCallDest of
                          Pil.CallAddr constFuncPtrOp -> Right constFuncPtrOp
                          Pil.CallFunc destFunc -> Left $ simpFunc destFunc
                          e -> error $ "Unexpected PLT dest: " <> show e
                      )
                    )
                 )
          . HashMap.toList
          $ pltFuncMapping
        expected :: HashMap SimpFunc (SimpFunc, Either SimpFunc Pil.ConstFuncPtrOp)
        expected = HashMap.fromList
          [ ( ("foo", 0x1040)
            , ( ("_foo", 0x1040)
              , Left ("foo", 0x1119)))
          , ( ("puts", 0x1030)
            , ( ("_puts", 0x1030)
              , Right (Pil.ConstFuncPtrOp 0x4000 $ Just "puts")))
          , ( ("__cxa_finalize", 0x1050)
            , ( ("___cxa_finalize", 0x1050)
              , Right (Pil.ConstFuncPtrOp 0x3fe0 $ Just "__cxa_finalize")))
          ]
    it "should generate a plt thunk mapping" $ do
      pltFuncMapping' `shouldBe` expected

    let sansPltThunks = HashMap.fromList
          . fmap (over _1 $ view #name)
          . CfgStore.purgePltThunks
          $ funcsWithCfgs
        
        extractedJenkinsCallDestFunc = fromJust $ do
          cfg <- HashMap.lookup "jenkins" sansPltThunks
          callNode <- headMay
            . mapMaybe (^? #_Call)
            . HashSet.toList
            . G.nodes
            $ cfg
          destFunc <- callNode ^? #callDest . #_CallFunc
          return (destFunc ^. #name, destFunc ^. #address)

        expected' = ("foo", 0x1119)
    
    it "should update callsites to PLT thunk to call actual func" $ do
      extractedJenkinsCallDestFunc `shouldBe` expected'                          

    store' <- runIO $ CfgStore.init Nothing bv
    callGraph <- runIO $ CfgStore.getCallGraph store'

    it "should have correct call graph" $ do
      let actual = sort
            . filter (not . Text.isPrefixOf "sub_" . fst . fst)
            . fmap (bimap simpFunc simpFunc . snd . G.toTupleLEdge)
            . G.edges
            $ callGraph
          expectedCallGraph = sort
            [ ( ("jenkins",  0x1141)
              , ("foo",  0x1119))
            , ( ("frame_dummy", 0x1110)
              , ("register_tm_clones", 0x1090))
            , ( ("__do_global_dtors_aux", 0x10d0)
              , ("deregister_tm_clones", 0x1060))
            , ( ("foo", 0x1119)
              , ("puts", 0x1030))
            , ( ("__do_global_dtors_aux", 0x10d0)
              , ("__cxa_finalize", 0x1050))
            ]
      PShow actual `shouldBe` PShow expectedCallGraph

    funcs <- runIO $ CfgStore.getFuncs store'

    it "Should prepend internal PTL thunk names with _" $ do
      let actual = sort
            . filter (not . Text.isPrefixOf "sub_" . fst)
            . fmap simpFunc
            $ funcs
          expected'' =
            [ ("__cxa_finalize", 0x1050)
            , ("__do_global_dtors_aux", 0x10d0)
            , ("_fini", 0x1158)
            , ("_foo", 0x1040)
            , ("_init", 0x1000)
            , ("deregister_tm_clones", 0x1060)
            , ("foo", 0x1119)
            , ("frame_dummy", 0x1110)
            , ("jenkins", 0x1141)
            , ("puts", 0x1030)
            , ("register_tm_clones", 0x1090)
            ]
      actual `shouldBe` expected''

  context "PLT Thunks (Ghidra)" $ do
    (bv :: GhidraImporter) <- unsafeFromRight <$> runIO (openBinary pltThunkLib)
    
    funcsWithCfgs <- runIO $ CfgStore.getFuncsWithCfgs bv
    
    let cfgMapping = HashMap.fromList funcsWithCfgs
        simpFunc :: Function -> SimpFunc
        simpFunc func = (func ^. #name, func ^. #address)
        pltFuncMapping = CfgStore.getPltThunkMapping funcsWithCfgs
        pltFuncMapping' :: HashMap SimpFunc (SimpFunc, Either SimpFunc Pil.ConstFuncPtrOp)
        pltFuncMapping' = HashMap.fromList
          . fmap (\(func, x) ->
                    ( simpFunc func
                    , ( simpFunc $ x ^. #renamedFunc
                      , case x ^. #newCallDest of
                          Pil.CallAddr constFuncPtrOp -> Right constFuncPtrOp
                          Pil.CallFunc destFunc -> Left $ simpFunc destFunc
                          e -> error $ "Unexpected PLT dest: " <> show e
                      )
                    )
                 )
          . HashMap.toList
          $ pltFuncMapping
        expected :: HashMap SimpFunc (SimpFunc, Either SimpFunc Pil.ConstFuncPtrOp)
        expected = HashMap.fromList
          [ ( ("foo", 0x101040)
            , ( ("_foo", 0x101040)
              , Left ("foo", 0x101119)))
          ]
    
    it "should have expected PLT thunk mapping" $ do
      PShow pltFuncMapping' `shouldBe` PShow expected
    
    it "should find a PLT thunk with internal call" $ do
      let k = ("foo", 0x101040)
      PShow (HashMap.lookup k pltFuncMapping')
        `shouldBe` PShow (HashMap.lookup k expected)

    it "should find a PLT thunk with external call (puts)" $ do
      let k = ("puts", 0x101030)
      PShow (HashMap.lookup k pltFuncMapping')
        `shouldBe` PShow (HashMap.lookup k expected)

    fooThunkFunc <- fmap fromJust . runIO $ CG.getFunction bv 0x101040
    fooFunc <- fmap fromJust . runIO $ CG.getFunction bv 0x101119
    let fooThunkCfg = fromJust $ HashMap.lookup fooThunkFunc cfgMapping
        expectedCallDest = Just $ Pil.CallFunc fooFunc          
    
    it "should identify thunk call to internal func" $ do
      CfgStore.getPltThunkDest fooThunkFunc fooThunkCfg `shouldBe` expectedCallDest

    let sansPltThunks = HashMap.fromList
          . fmap (over _1 $ view #name)
          . CfgStore.purgePltThunks
          $ funcsWithCfgs
        
        extractedJenkinsCallDestFunc = fromJust $ do
          cfg <- HashMap.lookup "jenkins" sansPltThunks
          callNode <- headMay
            . mapMaybe (^? #_Call)
            . HashSet.toList
            . G.nodes
            $ cfg
          destFunc <- callNode ^? #callDest . #_CallFunc
          return (destFunc ^. #name, destFunc ^. #address)

        expected' = ("foo", 0x101119)
    
    it "should update callsites to PLT thunk to call actual func" $ do
      PShow extractedJenkinsCallDestFunc `shouldBe` PShow expected'    

  context "shimmyFunc" $ do
    (bv :: BNImporter) <- unsafeFromRight <$> runIO (openBinary shimMainBndb)
    store <- runIO $ CfgStore.init Nothing bv

    strDupFunc <- runIO . getFunction bv $ FuncSym "strdup"
    strDupCfg <- fmap fromJust . runIO $ CfgStore.getFuncCfgInfo store strDupFunc

    it "should find the original strdup function stub" $ do
      HashSet.size (G.nodes $ strDupCfg ^. #cfg) `shouldBe` 2

    funcNameMappingPreShim <- runIO $ CfgStore.getFuncNameMapping store
   
    (bvShim :: BNImporter) <- unsafeFromRight <$> runIO (openBinary shimLibCBndb)
    strDupFuncShim <- runIO . getFunction bvShim $ FuncAddr 0x154c
    strLenFuncShim <- runIO . getFunction bvShim $ FuncAddr 0x13b6

    -- strdup is Shimmed into store!
    runIO $ CfgStore.shimmyFunc bvShim store strDupFunc strDupFuncShim

    strDupCfgShim <- fmap fromJust . runIO $ CfgStore.getFuncCfgInfo store strDupFunc

    it "should replace strdup cfg info with shimmed version" $ do
      HashSet.size (G.nodes $ strDupCfgShim ^. #cfg) `shouldBe` 12

    funcNameMappingPostShim <- runIO $ CfgStore.getFuncNameMapping store

    it "should insert function of inner call in shimmed func cfg into store" $ do
      HashMap.member "strlen" funcNameMappingPreShim  `shouldBe` False
      HashMap.member "strlen" funcNameMappingPostShim `shouldBe` True

    let strLenFunc = fromJust $ HashMap.lookup "strlen" funcNameMappingPostShim

    -- strlen is Shimmed into store!
    runIO $ CfgStore.shimmyFunc bvShim store strLenFunc strLenFuncShim

    strLenCfgShim <- fmap fromJust . runIO $ CfgStore.getFuncCfgInfo store strLenFunc

    it "should replace strdup cfg info with shimmed version" $ do
      HashSet.size (G.nodes $ strLenCfgShim ^. #cfg) `shouldBe` 24
