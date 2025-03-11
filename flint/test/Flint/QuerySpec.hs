{- HLINT ignore "Evaluate" -}

module Flint.QuerySpec where

import Flint.Prelude hiding (sym, const, until)

import Flint.Analysis.Path.Matcher
import Flint.Analysis.Path.Matcher.Primitives (getInitialPrimitives)
import qualified Flint.Analysis.Path.Matcher.Primitives.Library as PrimLib
import qualified Flint.Analysis.Path.Matcher.Primitives.Library.StdLib as StdLibPrims
import Flint.Cfg.Path (samplesFromQuery)
import qualified Flint.Cfg.Store as Store
import Flint.Query
import Flint.Types.Analysis.Path.Matcher.Primitives (CallablePrimitive)
import qualified Flint.Types.CachedMap as CM
import Flint.Types.Cfg.Store (CfgStore)

import Blaze.Import.Binary (BinaryImporter(openBinary))
import qualified Blaze.Import.CallGraph as CG
import Blaze.Import.Source.Ghidra (GhidraImporter)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Test.Hspec


libatm2 :: FilePath
libatm2 = "res/test_bins/onion_atm/libatm2.so"

dirtyBenchmark :: FilePath
dirtyBenchmark = "res/test_bins/dirty_benchmark/dirty_benchmark"

data TestCtx = TestCtx
  { atmImp :: GhidraImporter
  , atmStore :: CfgStore
  , dirtyImp :: GhidraImporter
  , dirtyStore :: CfgStore
  } deriving (Generic)

getTestCtx :: IO TestCtx
getTestCtx = do
  (atmImp :: GhidraImporter) <- unsafeFromRight <$> openBinary libatm2
  atmStore <- Store.init Nothing atmImp
  (dirtyImp :: GhidraImporter) <- unsafeFromRight <$> openBinary dirtyBenchmark
  dirtyStore <- Store.init Nothing dirtyImp

  return $ TestCtx
    { atmImp = atmImp
    , atmStore = atmStore
    , dirtyImp = dirtyImp
    , dirtyStore = dirtyStore
    }


spec :: Spec
spec = beforeAll getTestCtx . describe "Flint.Query" $ do
  context "onionFlow" $ do
    let simpleFunc x = (x ^. #name, x ^. #address)
    it "should find initial callable prims for controlled format string prim" $ \tctx -> do
      let stdLibPrims = StdLibPrims.controlledFormatStringPrims
          action = do
            funcs <- Store.getFuncs $ tctx ^. #dirtyStore
            let cprims = getInitialPrimitives stdLibPrims funcs

            return $ do
              (s :: HashSet CallablePrimitive) <- HashMap.lookup PrimLib.controlledFormatString cprims
              return
                . HashSet.fromList
                . fmap (simpleFunc . view #func)
                . HashSet.toList
                $ s
          expected = Just $ HashSet.fromList
            [("sprintf",Address 0x101350),("printf",Address 0x101270)]

      action `shouldReturn` expected

    it "should find fmt string prim in func with direct format string prim" $ \tctx -> do
      let stdLibPrims = StdLibPrims.controlledFormatStringPrims
          action = do
            funcs <- Store.getFuncs $ tctx ^. #dirtyStore
            func <- fromJust <$> CG.getFunction (tctx ^. #dirtyImp) 0x0101d88
            let q = QueryExpandAll $ QueryExpandAllOpts
                  { callExpandDepthLimit = 0
                  , numSamples = 1
                  }
            [path] <- samplesFromQuery (tctx ^. #dirtyStore) func q
            let initialCallablePrims = getInitialPrimitives stdLibPrims funcs
                pprep = mkPathPrep [] path

            r <- matchAndReturnCallablePrim (chooseSolver False) initialCallablePrims func pprep PrimLib.controlledFormatStringPrim
            return r
          expected = Just . Right $ PrimLib.controlledFormatString

      (fmap (fmap $ view #prim) <$> action) `shouldReturn` expected

      
    it "dirty_benchmark" $ \tctx -> do
      let stdLibPrims = StdLibPrims.controlledFormatStringPrims
          prims = [PrimLib.controlledFormatStringPrim]
          action = do
            onionFlow False 3 (tctx ^. #dirtyStore) stdLibPrims prims
            m <- CM.getSnapshot $ tctx ^. #dirtyStore . #callablePrims
            return $ do
              s <- HashMap.lookup PrimLib.controlledFormatString m
              return $ HashSet.map (view #name . view #func) s
          expected = Just $ HashSet.fromList
            [ "printf", "sprintf", "call_format_string_vulnerability", "format_string_vulnerability" ]

      action `shouldReturn` expected
