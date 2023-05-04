{- HLINT ignore "Redundant do" -}

module Blaze.Cfg.Path.SolverSpec where

import Blaze.Prelude

import qualified Binja.Core as BN
import qualified Blaze.Import.CallGraph as Cgi
import qualified Blaze.Import.Cfg as Cfgi
import qualified Blaze.Cfg.Path as Path

import Blaze.Types.Import (ImportResult(ImportResult))
import qualified Blaze.Import.Source.BinaryNinja as Bni
import Blaze.Cfg.Path.Solver (solvePaths)

import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.bndb"

dungeonBin :: FilePath
dungeonBin = "res/test_bins/Dungeon_Master/Dungeon_Master.bndb"

spec :: Spec
spec = describe "Blaze.Cfg.Path.SolverSpec" $ do
  context "solvePath" $ do
    context "Dive_Logger" $ do
      bv <- unsafeFromRight <$> runIO (BN.getBinaryView diveBin)
      runIO $ BN.updateAnalysisAndWait bv
      let importer = Bni.BNImporter bv
    
      context "selectDive" $ do
        selectDiveFunc <- fmap fromJust . runIO $ Cgi.getFunction importer 0x804e080
        (ImportResult _ctx _mapping selectDiveCfg) <- runIO $ fromJust <$> Cfgi.getCfg importer selectDiveFunc 0
        let paths = Path.getAllSimplePaths selectDiveCfg
        it "should get the correct number of simple paths" $ do
          length paths `shouldBe` 9

        r <- runIO $ solvePaths paths

        it "should solve all simple paths" $
          ( length $ r ^. #satPaths
          , length $ r ^. #unsatPaths
          , length $ r ^. #unkPaths) `shouldBe` (5, 4, 0)

      context "MainMenu" $ do
        mainMenuFunc <- fmap fromJust . runIO $ Cgi.getFunction importer 0x804ce80
        (ImportResult _ctx _mapping mainMenuCfg) <- runIO $ fromJust <$> Cfgi.getCfg importer mainMenuFunc 0
        let paths = Path.getAllSimplePaths mainMenuCfg
        it "should get the correct number of simple paths" $ do
          length paths `shouldBe` 11
      
        r <- runIO $ solvePaths paths

        -- -- Uncomment to print out paths.
        -- runIO $ do
        --   prettySats r
        --   putText "\n\n"
        --   prettyUnsats r

        it "should solve all simple paths" $
          ( length $ r ^. #satPaths
          , length $ r ^. #unsatPaths
          , length $ r ^. #unkPaths) `shouldBe` (9, 2, 0)

    context "Dungeon_Master" $ do
      bv <- unsafeFromRight <$> runIO (BN.getBinaryView dungeonBin)
      runIO $ BN.updateAnalysisAndWait bv
      let importer = Bni.BNImporter bv
    
      context "cgc_sendCurrentDungeonView" $ do
        sendCurrentDungeonViewFunc <- fmap fromJust . runIO $ Cgi.getFunction importer 0x804d150
        (ImportResult _ctx _mapping sendCurrentDungeonViewCfg) <- runIO $ fromJust <$> Cfgi.getCfg importer sendCurrentDungeonViewFunc 0
        let paths = Path.getAllSimplePaths sendCurrentDungeonViewCfg
        it "should get the correct number of simple paths" $ do
          length paths `shouldBe` 32

        r <- runIO $ solvePaths paths

        -- -- Uncomment to print out paths
        -- runIO $ do
        --   prettySats r
        --   putText "\n\n"
        --   prettyUnsats r

        it "should solve all simple paths" $
          ( length $ r ^. #satPaths
          , length $ r ^. #unsatPaths
          , length $ r ^. #unkPaths
          , length $ r ^. #constraintGenErrorPaths
          , length $ r ^. #solverErrorPaths)
          `shouldBe`
          (4, 8, 0, 0, 20)
