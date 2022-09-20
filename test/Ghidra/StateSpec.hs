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

  context "handles errors" $ do
    egs1 <- runIO . runGhidra $ State.openDatabase "/tmp/hopefullydoesnotexist"
    it "should be unable to load binary without options" $ do
      egs1 `shouldBe` (Left State.ImportByUsingBestGuessError)

    let badlang = "langdoesnotexist"
    egs2 <- runIO . runGhidra $ do
      let opts = State.OpenDatabaseOptions
            { compiler = Nothing
            , language = Just badlang
            , quiet = True
            }
      State.openDatabase' opts a1Bin
    it "should be unable to load binary with options" $ do
      egs2 `shouldBe` (Left $ State.CouldNotFindLang badlang)
