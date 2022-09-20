module Ghidra.StateSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import Ghidra.Core
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1"

spec :: Spec
spec = describe "Ghidra.State" $ do
  context "loads a1 test binary" $ do
    egs <- runIO . runGhidra $ State.openDatabase a1Bin
    it "should load binary" $ do
      isRight egs `shouldBe` True

    hasAnalyzed <- case egs of
      Left err -> error $ "Failed to open a1 binary: " <> show err
      Right gs -> runIO . runGhidra $ do
        State.analyze gs
        State.hasBeenAnalyzed gs
    
    it "should analyze binary" $ do
      hasAnalyzed `shouldBe` True

    -- b <- runIO . runGhidra $ do
    --   gs <- State.openDatabase a1Bin >>= State.analyze
    --   isNil' $ gs ^. #unGhidraState
    -- it "should be able to load it again after closing the JVM" $ do
    --   b `shouldBe` False

    -- b2 <- runIO . runGhidra $ do
    --   let openDbOpts = State.defaultOpenDatabaseOptions
    --         & #quiet .~ Just True
    --   gs <- State.openDatabase' (Just openDbOpts) a1Bin >>= State.analyze
    --   isNil' $ gs ^. #unGhidraState
    -- it "should load and analyze binary with options" $ do
    --   b2 `shouldBe` False
