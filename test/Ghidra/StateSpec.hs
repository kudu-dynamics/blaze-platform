module Ghidra.StateSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import System.IO.Temp (getCanonicalTemporaryDirectory)
import System.Directory (doesFileExist)
import Ghidra.Core
import Test.Hspec
import qualified Data.Text as Text


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1"

spec :: Spec
spec = describe "Ghidra.State" $ do
  context "loads a1 test binary" $ do
    egs <- runIO . runGhidraOrError $ State.openDatabase a1Bin
    it "should load binary" $ do
      isRight egs `shouldBe` True

    hasAnalyzed <- case egs of
      Left err -> error $ "Failed to open a1 binary: " <> show err
      Right gs -> runIO . runGhidraOrError $ do
        State.analyze gs
        State.hasBeenAnalyzed gs

    it "should analyze binary" $ do
      hasAnalyzed `shouldBe` True

    (tempFilePath, dbFileExists) <- runIO $ do
      n :: Word16 <- randomIO
      tempDir <- getCanonicalTemporaryDirectory
      let tempFilePath = tempDir <> "/" <> "ghidra_a1" <> show n <> ".gzf"
      let gs = unsafeFromRight egs
      State.saveDatabase gs tempFilePath
      exists <- doesFileExist tempFilePath
      return (tempFilePath, exists)

    it "should save analysis database" $ do
      dbFileExists `shouldBe` True

    egs' <- runIO . runGhidraOrError $ State.openDatabase tempFilePath

    it "should load saved analysis database" $ do
      isRight egs' `shouldBe` True


  context "handles errors" $ do
    egs1 <- runIO $ runGhidra (State.openDatabase "/tmp/hopefullydoesnotexist")
    it "should be unable to load binary without options" $ do
      egs1 `shouldSatisfy` \case
        Left msg -> "java.io.FileNotFoundException" `Text.isPrefixOf` msg
        Right _ -> False

    let badlang = "langdoesnotexist"
    egs2 <- runIO . runGhidraOrError $ do
      let opts = State.OpenDatabaseOptions
            { compiler = Nothing
            , language = Just badlang
            , quiet = True
            }
      State.openDatabase' opts a1Bin
    it "should be unable to load binary with options" $ do
      egs2 `shouldBe` Left (State.CouldNotFindLang badlang)
