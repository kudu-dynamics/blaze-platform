{- HLINT ignore "Redundant do" -}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Flint.Cfg.PathSpec where

import Flint.Prelude

import Flint.Analysis (addCfgStoreForBinary)
import Flint.Cfg.Path
import qualified Flint.Cfg.Store as CfgStore
import Flint.Types.Query (getFunction, FuncConfig(FuncSym))

import Blaze.Path (SampleRandomPathError')
import Blaze.Import.Binary (BinaryImporter(openBinary))
import qualified Blaze.Import.CallGraph as Cg
import Blaze.Import.Source.BinaryNinja (BNImporter)
import qualified Blaze.Types.Path as P
import Blaze.Cfg.Path (PilPath)
import Blaze.Types.Pil (Stmt)
import Blaze.Types.Cfg (PilNode, CallNode)
import qualified Blaze.Types.Cfg as Cfg

import qualified Data.HashSet as HashSet
import Test.Hspec


interCfgBndb :: FilePath
interCfgBndb = "res/test_bins/intercfg/intercfg.bndb"

diveLoggerBndb :: FilePath
diveLoggerBndb = "res/test_bins/Dive_Logger/Dive_Logger.bndb"

spec :: Spec
spec = describe "Flint.Cfg.Path" $ do
  (bv :: BNImporter) <- unsafeFromRight <$> runIO (openBinary interCfgBndb)
  store <- runIO $ do
    allFuncs <- Cg.getFunctions bv
    cfgStore <- CfgStore.init Nothing bv
    addCfgStoreForBinary bv allFuncs cfgStore
    return cfgStore

  singlePathFunc <- runIO . getFunction bv $ FuncSym "single_path_no_calls"
  outerAFunc <- runIO . getFunction bv $ FuncSym "outer_a"
  innerFunc <- runIO . getFunction bv $ FuncSym "inner"
  mainFunc <- runIO . getFunction bv $ FuncSym "main"
    
  let alwaysLowestOfRange (a, _) = return a
      _alwaysHighestOfRange (_, b) = return b
      _alwaysZero :: Int -> IO Int
      _alwaysZero _ = return 0
        
      getPathNodeCount :: PilPath -> Int
      getPathNodeCount = length . HashSet.toList . P.nodes
      _getSinglePath :: [PilPath] -> PilPath
      _getSinglePath = \case
        [] -> error "Should contain one path. Got zero."
        [x] -> x
        xs -> error $ "Should contain one path, got " <> show xs <> "."
      getPathNodes :: PilPath -> [PilNode]
      getPathNodes = HashSet.toList . P.nodes
      getCallDestName :: CallNode [Stmt] -> Maybe Text
      getCallDestName n = n ^? #callDest . #_CallFunc . #name
      _hasCallDestTo :: Text -> CallNode [Stmt] -> Bool
      _hasCallDestTo calleeName n = Just calleeName == getCallDestName n
      getCalleeFuncName :: PilNode -> Maybe Text
      getCalleeFuncName = \case
        Cfg.Call n -> getCallDestName n
        _ -> Nothing
      isCallTo :: Text -> PilNode -> Bool
      isCallTo calleeName n = Just calleeName == getCalleeFuncName n

  context "exploreFromStartingFunc_" $ do    
    it "should get path from func with single basic block" $ do
      let startFunc = singlePathFunc
          expansionChooser = expandAllToDepth 0

          action :: IO (Maybe PilPath)
          action = exploreFromStartingFunc_ alwaysLowestOfRange expansionChooser 0 store startFunc
          modifyResult :: Maybe PilPath -> Int
          modifyResult = getPathNodeCount . fromJust
          expected = 1
      (modifyResult <$> action) `shouldReturn` expected

    it "should not expand call nodes if expand limit is 0" $ do
      let startFunc = outerAFunc
          expansionChooser = expandAllToDepth 0

          action :: IO (Maybe PilPath)
          action = exploreFromStartingFunc_ alwaysLowestOfRange expansionChooser 0 store startFunc
          modifyResult :: Maybe PilPath -> Bool
          modifyResult
            = any (isCallTo "inner")
            . getPathNodes
            . fromJust
          expected = True
      (modifyResult <$> action) `shouldReturn` expected

    it "should expand all call nodes once when ExpandAllCallsAtEachLevel strategy and expand limit is 1" $ do
      let startFunc = mainFunc
          expansionChooser = expandAllToDepth 1

          action :: IO (Maybe PilPath)
          action = exploreFromStartingFunc_ alwaysLowestOfRange expansionChooser 0 store startFunc
          modifyResult :: Maybe PilPath -> Int
          modifyResult
            = length
            . filter (isCallTo "inner")
            . getPathNodes
            . fromJust
          expected = 2
      (modifyResult <$> action) `shouldReturn` expected

  context "exploreForward_ expandAllStrategy" $ do
    it "should get path from func with single basic block" $ do
      let startFunc = singlePathFunc
          strat = expandAllStrategy alwaysLowestOfRange 0 store

          action :: IO (Either (SampleRandomPathError' PilNode) (Maybe PilPath))
          action = runExceptT $ exploreForward_
            (const randomIO)
            strat
            0
            startFunc
          modifyResult :: Either (SampleRandomPathError' PilNode) (Maybe PilPath)
                       -> Int
          modifyResult = getPathNodeCount
              . fromJust . unsafeFromRight
          expected = 1
      (modifyResult <$> action) `shouldReturn` expected

    it "should not expand call nodes if expand limit is 0" $ do
      let startFunc = outerAFunc
          strat = expandAllStrategy alwaysLowestOfRange 0 store

          action :: IO (Either (SampleRandomPathError' PilNode) (Maybe PilPath))
          action = runExceptT $ exploreForward_
            (const randomIO)
            strat
            0
            startFunc
          modifyResult :: Either (SampleRandomPathError' PilNode) (Maybe PilPath)
                       -> Bool
          modifyResult
            = any (isCallTo "inner")
            . getPathNodes
            . fromJust
            . unsafeFromRight
          expected = True
      (modifyResult <$> action) `shouldReturn` expected

    it "should expand all call nodes once when expand limit is 1" $ do
      let startFunc = mainFunc
          strat = expandAllStrategy alwaysLowestOfRange 1 store

          action :: IO (Either (SampleRandomPathError' PilNode) (Maybe PilPath))
          action = runExceptT $ exploreForward_
            (const randomIO)
            strat
            0
            startFunc
          modifyResult :: Either (SampleRandomPathError' PilNode) (Maybe PilPath)
                       -> Int
          modifyResult
            = length
            . filter (isCallTo "inner")
            . getPathNodes
            . fromJust
            . unsafeFromRight
          expected = 2
      (modifyResult <$> action) `shouldReturn` expected

    it "should expand every call two times when expand limit is 2" $ do
      let startFunc = mainFunc
          strat = expandAllStrategy alwaysLowestOfRange 2 store

          action :: IO (Either (SampleRandomPathError' PilNode) (Maybe PilPath))
          action = runExceptT $ exploreForward_
            (const randomIO)
            strat
            0
            startFunc
          modifyResult :: Either (SampleRandomPathError' PilNode) (Maybe PilPath)
                       -> Int
          modifyResult
            = length
            . filter (isCallTo "puts")
            . getPathNodes
            . fromJust
            . unsafeFromRight
          expected = 2
      (modifyResult <$> action) `shouldReturn` expected


  context "exploreForward_ expandToTargetsStrategy" $ do

    targetInSinglePathFunc <- runIO
      . mkExpandToTargetsStrategy alwaysLowestOfRange 10 store
      $ (singlePathFunc, 0x11a6) :| []

    it "should get path from func with single block, where target is in that block" $ do
      let startFunc = singlePathFunc
          strat = targetInSinglePathFunc

          action :: IO (Either (SampleRandomPathError' PilNode) (Maybe PilPath))
          action = runExceptT $ exploreForward_
            (const randomIO)
            strat
            0
            startFunc
          modifyResult :: Either (SampleRandomPathError' PilNode) (Maybe PilPath)
                       -> Int
          modifyResult = getPathNodeCount
              . fromJust . unsafeFromRight
          expected = 1
      (modifyResult <$> action) `shouldReturn` expected

    targetInInner <- runIO
      . mkExpandToTargetsStrategy alwaysLowestOfRange 10 store
      $ (innerFunc, 0x1154) :| []

    it "should find target a couple calls away" $ do
      let startFunc = mainFunc
          strat = targetInInner

          action :: IO (Either (SampleRandomPathError' PilNode) (Maybe PilPath))
          action = runExceptT $ exploreForward_
            (const randomIO)
            strat
            0
            startFunc
          modifyResult :: Either (SampleRandomPathError' PilNode) (Maybe PilPath)
                       -> Bool
          modifyResult
            = any (isCallTo "puts")
            . getPathNodes
            . fromJust
            . unsafeFromRight
          expected = True
      (modifyResult <$> action) `shouldReturn` expected
