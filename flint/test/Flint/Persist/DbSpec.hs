{- HLINT ignore "Redundant do" -}

module Flint.Persist.DbSpec where

import Flint.Prelude

import qualified Flint.Cfg.Store as Store

import Blaze.CallGraph (getCallGraph)
import Blaze.Import.Binary (openBinary)
import Blaze.Import.Source.BinaryNinja (BNImporter)
import qualified Blaze.Persist.Db as Db
import qualified Blaze.Types.Graph as G

import qualified Data.HashSet as HashSet
import System.IO.Temp (withSystemTempFile)

import Test.Hspec


spec :: Spec
spec = describe "Flint.Persist.Db" $ do
  context "CallGraph" $ do
    (bv :: BNImporter) <- fmap unsafeFromRight . runIO $ openBinary "res/test_bins/Dive_Logger/Dive_Logger.bndb"
    store <- runIO $ Store.init Nothing bv
    let funcs = store ^. #funcs
    cg <- runIO $ getCallGraph bv funcs
    mCg' <- runIO $ withSystemTempFile "dive_callgraph.flint" $ \fp _ -> do
      conn <- Db.init fp
      Db.insertCallGraph conn cg
      Db.loadCallGraph conn

    let reduceGraph g = ( sort . fmap (view #name) . HashSet.toList $ G.nodes g
                        , sort $ G.edges g
                        )

    it "should store and load CallGraph" $ do
      reduceGraph <$> mCg' `shouldBe` Just (reduceGraph cg)
