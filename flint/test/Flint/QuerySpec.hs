{- HLINT ignore "Evaluate" -}

module Flint.QuerySpec where

import Flint.Prelude hiding (sym, const, until)

import Flint.Analysis.Path.Matcher
import Flint.Analysis.Path.Matcher.Primitives (getInitialPrimitives)
import qualified Flint.Analysis.Path.Matcher.Primitives.Library as PrimLib
import qualified Flint.Analysis.Path.Matcher.Primitives.Library.StdLib as StdLibPrims
import qualified Flint.Cfg.Store as Store
import Flint.Query
import Flint.Types.Analysis.Path.Matcher.Primitives (CallablePrimitive)
import qualified Flint.Types.CachedMap as CM
import Flint.Types.Cfg.Store (CfgStore)

import Blaze.Import.Binary (BinaryImporter(openBinary))
import qualified Blaze.Import.CallGraph as CG
import Blaze.Import.Source.Ghidra (GhidraImporter)
import Blaze.Types.Function (Function(Function))
import qualified Blaze.Types.Graph as G

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
    it "should find initial callable prims for controlled format string prim" $ \tctx -> do
      let stdLibPrims = StdLibPrims.controlledFormatStringPrims
          simpleFunc x = (x ^. #name, x ^. #address)
          action = do
            funcs <- Store.getFuncs $ tctx ^. #dirtyStore
            funcs' <- CG.getFunctions $ tctx ^. #dirtyImp
            funcs'' <- fmap (fmap fst) . Store.getFuncsWithCfgs $ tctx ^. #dirtyImp
            pprint . sort $ simpleFunc <$> funcs'
            pprint . sort $ simpleFunc <$> funcs''
            let cprims = getInitialPrimitives stdLibPrims funcs
            pprint cprims

            -- Store.populateInitialPrimitives stdLibPrims $ tctx ^. #dirtyStore
            -- cprims <- CM.getSnapshot $ tctx ^. #dirtyStore . #callablePrims
            return $ do
              (s :: HashSet CallablePrimitive) <- HashMap.lookup PrimLib.controlledFormatString cprims
              return
                . HashSet.fromList
                . fmap ((\func -> (func ^. #name, func ^. #address)) . view #func)
                . HashSet.toList
                $ s
          expected = Just $ HashSet.fromList
            []

      action `shouldReturn` expected
    -- it "should find fmt string prim in func with direct format string prim" $ \tctx -> do
    --   -- TODO: matchAndReturnCallablePrim for single path
    --   let stdLibPrims = StdLibPrims.controlledFormatStringPrims
    --       prims = [PrimLib.controlledFormatStringPrim]
    --       action = do
    --         func <- fromJust <$> CG.getFunction (tctx ^. #dirtyImp) 0x0101d88
    --         let q = QueryExpandAll $ QueryExpandAllOpts
    --               { callExpandDepthLimit = 1
    --               , numSamples = 1
    --               }
    --         [path] <- samplesFromQuery (tctx ^. #dirtyStore) func q
    --         initialCallablePrims <- 
    --         matchAndReturnCallablePrim (chooseSolver False) 
    --         onionFlow False 3 (tctx ^. #dirtyStore) stdLibPrims prims
    --         CM.getSnapshot $ tctx ^. #dirtyStore . #callablePrims
    --       expected = HashMap.fromList [undefined]

    --   action `shouldReturn` expected

      
    -- it "dirty_benchmark" $ \tctx -> do
      
    --   -- TODO: matchAndReturnCallablePrim for single path
    --   let stdLibPrims = StdLibPrims.controlledFormatStringPrims
    --       prims = [PrimLib.controlledFormatStringPrim]
    --       action = do
    --         onionFlow False 3 (tctx ^. #dirtyStore) stdLibPrims prims
    --         CM.getSnapshot $ tctx ^. #dirtyStore . #callablePrims
    --       expected = HashMap.fromList [undefined]

    --   action `shouldReturn` expected
