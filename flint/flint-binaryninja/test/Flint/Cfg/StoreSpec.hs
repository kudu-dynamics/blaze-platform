module Flint.Cfg.StoreSpec where

import Flint.Prelude

import qualified Flint.Cfg.Store as CfgStore
import Flint.Types.Query (getFunction, FuncConfig(FuncSym, FuncAddr))

import Blaze.Import.Binary (BinaryImporter(openBinary))
import Blaze.Import.Source.BinaryNinja (BNImporter)
import qualified Blaze.Types.Graph as G

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Test.Hspec


shimMainBndb :: FilePath
shimMainBndb = "res/test_bins/shim/shim_main.bndb"

-- Contains strlen and strchr
shimLibCBndb :: FilePath
shimLibCBndb = "res/test_bins/shim/shim_libc.so.bndb"

spec :: Spec
spec = describe "Flint.Cfg.Store" $ do
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
    
