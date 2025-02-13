{- HLINT ignore "Redundant do" -}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Flint.Cfg.PathSpec where

import Flint.Prelude

import Flint.Analysis (addCfgStoreForBinary)
import Flint.Cfg.Path
import Flint.Types.Cfg.Store (CfgStore)
import qualified Flint.Cfg.Store as CfgStore
import Flint.Types.Query (getFunction, FuncConfig(FuncSym))

import Blaze.Path (SampleRandomPathError')
import Blaze.Import.Binary (BinaryImporter(openBinary))
import qualified Blaze.Import.CallGraph as Cg
import Blaze.Import.Source.BinaryNinja (BNImporter)
import qualified Blaze.Types.Path as P
import Blaze.Cfg.Path (PilPath)
import Blaze.Types.Function (Function)
import Blaze.Types.Pil (Stmt)
import Blaze.Types.Cfg (PilNode, CallNode)
import qualified Blaze.Types.Cfg as Cfg

import qualified Data.HashSet as HashSet
import Test.Hspec


interCfgBndb :: FilePath
interCfgBndb = "res/test_bins/intercfg/intercfg.bndb"

diveLoggerBndb :: FilePath
diveLoggerBndb = "res/test_bins/Dive_Logger/Dive_Logger.bndb"

data TestCtx = TestCtx
  { bv :: BNImporter
  , store :: CfgStore
  , singlePathFunc :: Function
  , outerAFunc :: Function
  , innerFunc :: Function
  , mainFunc :: Function
  , targetInSinglePathFunc :: ExplorationStrategy CallDepth (SampleRandomPathError' PilNode) IO
  , targetInInner :: ExplorationStrategy CallDepth (SampleRandomPathError' PilNode) IO
  } deriving (Generic)

spec :: Spec
spec = do
  let getTestCtx = do
        -- TODO: use GhidraImporter
        (bv :: BNImporter) <- unsafeFromRight <$> openBinary interCfgBndb
        store <- do
          allFuncs <- Cg.getFunctions bv
          cfgStore <- CfgStore.init Nothing bv
          addCfgStoreForBinary bv allFuncs cfgStore
          return cfgStore
        singlePathFunc <- getFunction bv $ FuncSym "single_path_no_calls"
        outerAFunc <- getFunction bv $ FuncSym "outer_a"
        innerFunc <- getFunction bv $ FuncSym "inner"
        mainFunc <- getFunction bv $ FuncSym "main"
        targetInSinglePathFunc <-
          mkExpandToTargetsStrategy alwaysLowestOfRange 10 store
          $ (singlePathFunc, 0x11a6) :| []
        targetInInner <-
          mkExpandToTargetsStrategy alwaysLowestOfRange 10 store
          $ (innerFunc, 0x1154) :| []

        return $ TestCtx
          { bv = bv
          , store = store
          , singlePathFunc = singlePathFunc
          , outerAFunc = outerAFunc
          , innerFunc = innerFunc
          , mainFunc = mainFunc
          , targetInSinglePathFunc = targetInSinglePathFunc
          , targetInInner = targetInInner
          }
      alwaysLowestOfRange (a, _) = return a
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

  beforeAll getTestCtx . describe "Flint.Cfg.Path" $ do
    context "exploreFromStartingFunc_" $ do    
      it "should get path from func with single basic block" $ \tctx -> do
        let startFunc = tctx ^. #singlePathFunc
            expansionChooser = expandAllToDepth 0

            action :: IO (Maybe PilPath)
            action = exploreFromStartingFunc_ alwaysLowestOfRange expansionChooser 0 (tctx ^. #store) startFunc
            modifyResult :: Maybe PilPath -> Int
            modifyResult = getPathNodeCount . fromJust
            expected = 1
        (modifyResult <$> action) `shouldReturn` expected

      it "should not expand call nodes if expand limit is 0" $ \tctx -> do
        let startFunc = tctx ^. #outerAFunc
            expansionChooser = expandAllToDepth 0

            action :: IO (Maybe PilPath)
            action = exploreFromStartingFunc_ alwaysLowestOfRange expansionChooser 0 (tctx ^. #store) startFunc
            modifyResult :: Maybe PilPath -> Bool
            modifyResult
              = any (isCallTo "inner")
              . getPathNodes
              . fromJust
            expected = True
        (modifyResult <$> action) `shouldReturn` expected

      it "should expand all call nodes once when ExpandAllCallsAtEachLevel strategy and expand limit is 1" $ \tctx -> do
        let startFunc = tctx ^. #mainFunc
            expansionChooser = expandAllToDepth 1

            action :: IO (Maybe PilPath)
            action = exploreFromStartingFunc_ alwaysLowestOfRange expansionChooser 0 (tctx ^. #store) startFunc
            modifyResult :: Maybe PilPath -> Int
            modifyResult
              = length
              . filter (isCallTo "inner")
              . getPathNodes
              . fromJust
            expected = 2
        (modifyResult <$> action) `shouldReturn` expected

    context "exploreForward_ expandAllStrategy" $ do
      it "should get path from func with single basic block" $ \tctx -> do
        let startFunc = tctx ^. #singlePathFunc
            strat = expandAllStrategy alwaysLowestOfRange 0 $ tctx ^. #store

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

      it "should not expand call nodes if expand limit is 0" $ \tctx -> do
        let startFunc = tctx ^. #outerAFunc
            strat = expandAllStrategy alwaysLowestOfRange 0 $ tctx ^. #store

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

      it "should expand all call nodes once when expand limit is 1" $ \tctx -> do
        let startFunc = tctx ^. #mainFunc
            strat = expandAllStrategy alwaysLowestOfRange 1 $ tctx ^. #store

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

      it "should expand every call two times when expand limit is 2" $ \tctx -> do
        let startFunc = tctx ^. #mainFunc
            strat = expandAllStrategy alwaysLowestOfRange 2 $ tctx ^. #store

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
      
      it "should get path from func with single block, where target is in that block" $ \tctx -> do
        let startFunc = tctx ^. #singlePathFunc
            strat = tctx ^. #targetInSinglePathFunc

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

      it "should find target a couple calls away" $ \tctx -> do
        let startFunc = tctx ^. #mainFunc
            strat = tctx ^. #targetInInner

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
