module Ghidra.FunctionSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import Ghidra.Core
import Language.Clojure.Core
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1"

spec :: Spec
spec = describe "Ghidra.Function" $ do
  context "getFunctions" $ do
    funcs <- runIO . runGhidra $ do
      gs <- State.openDatabase a1Bin >>= State.analyze
      isNil' $ gs ^. #unGhidraState
      Function.getFunctions gs
    it "should get all functions for a1 binary" $ do
      length funcs `shouldBe` 37
