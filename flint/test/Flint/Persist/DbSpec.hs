{- HLINT ignore "Redundant do" -}

module Flint.Persist.DbSpec where

import Flint.Prelude

import qualified Flint.Cfg.Store as CfgStore
import Flint.Types.Cfg.Store (CfgStore)

import Blaze.CallGraph (getCallGraph, CallGraph)
import qualified Blaze.Import.CallGraph as CG
import Blaze.Import.Binary (openBinary)
import Blaze.Import.Source.Ghidra (GhidraImporter)
import qualified Blaze.Persist.Db as Db
import Blaze.Types.Function (Function)
import qualified Blaze.Types.Graph as G

import qualified Data.HashSet as HashSet
import System.IO.Temp (withSystemTempFile)

import Test.Hspec

diveLogger :: FilePath
diveLogger = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

data TestCtx = TestCtx
  { bv :: GhidraImporter
  , store :: CfgStore
  , allFuncs :: [Function]
  , cg :: CallGraph
  } deriving (Generic)

getTestCtx :: IO TestCtx
getTestCtx = do
  (bv :: GhidraImporter) <- unsafeFromRight <$> openBinary diveLogger
  store <- CfgStore.init Nothing bv
  allFuncs <- CG.getFunctions bv
  cg <- getCallGraph bv allFuncs
  return $ TestCtx
    { bv = bv
    , store = store
    , allFuncs = allFuncs
    , cg = cg
    }

spec :: Spec
spec = beforeAll getTestCtx . describe "Flint.Persist.Db" $ do
  context "CallGraph" $ do
    let reduceGraph g = ( sort . fmap (view #name) . HashSet.toList $ G.nodes g
                        , sort $ G.edges g
                        )

    it "should store and load CallGraph" $ \tctx -> do
      let action = withSystemTempFile "dive_callgraph.flint" $ \fp _ -> do
              conn <- Db.init fp
              Db.insertCallGraph conn $ tctx ^. #cg
              Db.loadCallGraph conn

      fmap reduceGraph <$> action `shouldReturn` Just (reduceGraph $ tctx ^. #cg)
