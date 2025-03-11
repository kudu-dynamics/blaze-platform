module Blaze.Import.Source.Ghidra.CallGraphSpec where

import Blaze.Prelude

import Blaze.Import.CallGraph (CallGraphImporter (getFunction))
import qualified Blaze.Import.Source.Ghidra as G
import qualified Blaze.Types.Function as Func

import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

spec :: Spec
spec = describe "Blaze.Import.Source.Ghidra.CallGraph" $ do
  context "Import Function" $ do
    importer <- runIO $ G.getImporter diveBin
    mFunc <- runIO $ getFunction importer 0x804d670 -- cgc_SetParam function
    it "should import a function by address" $ do
      mFunc ^? _Just . #name `shouldBe` Just "cgc_SetParam"

    let func = fromJust mFunc
        params = func ^. #params

    it "should get the correct number of params" $ do
      length (func ^. #params) `shouldBe` 3
    
    it "should name the params correctly" $ do
      let getParamName (Func.FuncParamInfo p) = Just $ p ^. #name
          getParamName _ = Nothing
          paramNames :: Maybe [Text]
          paramNames = traverse getParamName params
          expected = Just ["paramName", "param", "len"]

      paramNames `shouldBe` expected
