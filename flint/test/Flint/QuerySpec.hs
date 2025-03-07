{- HLINT ignore "Evaluate" -}

module Flint.QuerySpec where

import Flint.Prelude hiding (sym, const, until)

import Flint.Analysis.Path.Matcher
import qualified Flint.Cfg.Store as Store
import Flint.Query

import Blaze.Import.Binary (BinaryImporter(openBinary))
import Blaze.Import.Source.Ghidra (GhidraImporter)
import Blaze.Types.Function (Function(Function))

import qualified Blaze.Types.Graph as G

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
  atmStore <- CfgStore.init Nothing atmImp
  (dirtyImp :: GhidraImporter) <- unsafeFromRight <$> openBinary dirtyBenchmark
  dirtyStore <- CfgStore.init Nothing dirtyImp

  return $ TestCtx
    { atmImp = atmImp
    , atmStore = atmStore
    , dirtyImp = dirtyImp
    , dirtyStore = dirtyStore
    }


spec :: Spec
spec = beforeAll getTestCtx . describe "Flint.Query" $ do
  context "onionFlow" $ do
    it "dirty_benchmark" $ \tctx -> do
      let action = do
            onionFlow False 3 (tctx ^. #atmStore) 
