module Blaze.Import.Source.Ghidra.CallGraphSpec where

import Blaze.Prelude

import Blaze.Import.CallGraph (CallGraphImporter (getFunction))
import qualified Blaze.Import.Source.Ghidra as G
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
    it "should get the correct number of params" $ do
      length (func ^. #params) `shouldBe` 3
