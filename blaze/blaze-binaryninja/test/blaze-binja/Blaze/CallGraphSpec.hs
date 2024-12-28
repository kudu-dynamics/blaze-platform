{- HLINT ignore "Redundant do" -}

module Blaze.CallGraphSpec where

import Blaze.Prelude

import qualified Binja.Core as BN
import qualified Blaze.CallGraph as Cg
import qualified Blaze.Import.CallGraph as Cgi
import Blaze.Types.Function (FuncParamInfo(FuncParamInfo), ParamInfo(ParamInfo), Access(Unknown))
import qualified Blaze.Types.Graph as G
import qualified Blaze.Import.Source.BinaryNinja as Bni
import Test.Hspec
import qualified Data.Text as Text


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.bndb"

spec :: Spec
spec = describe "Blaze.CallGraph" $ do
  context "Dive_Logger" $ do
    bv <- unsafeFromRight <$> runIO (BN.getBinaryView diveBin)
    runIO $ BN.updateAnalysisAndWait bv
    let importer = Bni.BNImporter bv

    funcs <- runIO $ Cgi.getFunctions importer
    
    context "getFunctions" $ do     
      it "should get the correct number of functions" $ do
        length funcs `shouldBe` 94

    context "getCallGraph" $ do
      cg <- runIO $ Cg.getCallGraph importer funcs
      it "should load a call graph with the correct number of nodes" $ do
        length (G.nodes cg) `shouldBe` 94
      it "should load a call graph with the correct number of edges" $ do
        length (G.edges cg) `shouldBe` 155

    context "getFunction" $ do
      mCgcMemCpyFunc <- runIO $ Cgi.getFunction importer 0x8049de0
      it "should find a function" $ do
        isJust mCgcMemCpyFunc `shouldBe` True

      let func = fromJust mCgcMemCpyFunc
          params = func ^. #params
          params' = over (#_FuncParamInfo . #name) (Text.take 4) <$> params
          expectedParams =
            [ FuncParamInfo $ ParamInfo "arg1" Unknown
            , FuncParamInfo $ ParamInfo "arg2" Unknown
            , FuncParamInfo $ ParamInfo "arg3" Unknown
            ]
      it "should import the correct function params" $ do
        params' `shouldBe` expectedParams
