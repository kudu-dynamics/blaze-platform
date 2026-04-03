{- HLINT ignore "Evaluate" -}

module Flint.QuerySpec where

import Flint.Prelude hiding (sym, const, until)

import Flint.Analysis.Path.Matcher
import Flint.Analysis.Path.Matcher.Primitives (getInitialWMIsFromStore)
import qualified Flint.Analysis.Path.Matcher.Primitives.Library as PrimLib
import qualified Flint.Analysis.Path.Matcher.Primitives.Library.PrimSpec as PrimSpec
import qualified Flint.Analysis.Path.Matcher.Primitives.Library.StdLib as StdLibPrims
-- import Flint.Cfg.Path (samplesFromQuery)
import qualified Flint.Cfg.Store as Store
import Flint.Query
import Flint.Types.Analysis.Path.Matcher.Primitives (CallableWMI)
import qualified Flint.Types.CachedMap as CM
import Flint.Types.Cfg.Store (CfgStore)

import Blaze.Import.Binary (BinaryImporter(openBinary))
-- import qualified Blaze.Import.CallGraph as CG
import Blaze.Import.Source.Ghidra (GhidraImporter)
import Blaze.Types.Function (_name)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Test.Hspec


libatm2 :: FilePath
libatm2 = "res/test_bins/onion_atm/libatm2.so"

dirtyBenchmark :: FilePath
dirtyBenchmark = "res/test_bins/dirty_benchmark/dirty_benchmark.gzf"

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
    it "should find initial callable prims for controlled format string prim" $ \tctx -> do
      let stdLibPrims = StdLibPrims.controlledFormatStringPrims
          store = tctx ^. #dirtyStore
          action = do
            cprims <- getInitialWMIsFromStore (Store.getFuncs store) (Store.resolveFuncRef store) stdLibPrims
            let cprims' = asOldCallableWMIsMap cprims

            return $ do
              (s :: HashSet CallableWMI) <- HashMap.lookup PrimSpec.controlledFormatStringSpec cprims'
              return
                . HashSet.fromList
                . fmap (view $ #func . _name)
                . HashSet.toList
                $ s
          expected = Just $ HashSet.fromList
            ["sprintf","printf"]

      action `shouldReturn` expected

--    it "should find fmt string prim in func with direct format string prim" $ \tctx -> do
--      let stdLibPrims = StdLibPrims.controlledFormatStringPrims
--          action = do
--            funcs <- Store.getFuncs $ tctx ^. #dirtyStore
--            func <- (^?! _Just . #_Internal) <$> CG.getFunction (tctx ^. #dirtyImp) (intToAddr 0x0101d88)
--            let q = QueryExpandAll $ QueryExpandAllOpts
--                  { callExpandDepthLimit = 0
--                  , numSamples = 1
--                  }
--            [path] <- samplesFromQuery (tctx ^. #dirtyStore) func q
--            let initialCallablePrims = getInitialWMIs stdLibPrims funcs
--                pprep = mkPathPrep [] path :: PathPrep TypedStmt
--
--            r <- matchAndReturnCallablePrim 1 (chooseSolver False) initialCallablePrims func pprep PrimLib.controlledFormatStringPrim
--            return r
--          expected = [PrimSpec.controlledFormatStringSpec]
--
--      (fmap (view #prim) <$> action) `shouldReturn` expected

    context "onion primitives" $ do
      it "should find controlled format string in dirty" $ \tctx -> do
        let stdLibPrims = StdLibPrims.controlledFormatStringPrims
            prims = [PrimLib.controlledFormatStringPrim]
            action = do
              onionFlow 1 False 3 1.0 (tctx ^. #dirtyStore) stdLibPrims prims HashMap.empty True HashSet.empty 5 [] []
              m <- fmap asOldCallableWMIsMap . CM.getSnapshot $ tctx ^. #dirtyStore . #callablePrims
              return $ do
                s <- HashMap.lookup PrimSpec.controlledFormatStringSpec m
                return $ HashSet.map (view $ #func . _name) s
            expected = Just $ HashSet.fromList
              [ "printf", "sprintf", "call_format_string_vulnerability", "format_string_vulnerability" ]
        action `shouldReturn` expected

      it "should find heap frees in atm2" $ \tctx -> do
        let stdLibPrims = StdLibPrims.freeHeapPrims
            prims = [PrimLib.freeHeapPrim]
            action = do
              onionFlow 20 False 3 1.0 (tctx ^. #atmStore) stdLibPrims prims HashMap.empty True HashSet.empty 5 [] []
              m <- fmap asOldCallableWMIsMap . CM.getSnapshot $ tctx ^. #atmStore . #callablePrims
              return $ do
                s <- HashMap.lookup PrimSpec.freeHeapSpec m
                return $ HashSet.map (view $ #func . _name) s

        -- checks to see if it gets at least 3 results
        ((> 3) . HashSet.size <<$>> action) `shouldReturn` Just True
