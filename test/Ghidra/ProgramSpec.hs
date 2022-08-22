module Ghidra.ProgramSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import qualified Ghidra.Program as Prog
import Ghidra.Core
import Language.Clojure.Core
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1"

spec :: Spec
spec = describe "Ghidra.Program" $ do
  -- gs <- runIO . runGhidra $ do
  --   gs <- State.openDatabase a1Bin >>= State.analyze
  --   b <- isNil' $ gs ^. #unGhidraState
  --   when b $ error "Couldn't open a1"
  --   return gs

  return ()
--   context "getConstantAddress" $ do
--     funcs <- runIO . runGhidra $ Function.getFunctions gs
--     it "should get all functions for a1 binary" $ do
--       length funcs `shouldBe` 37

