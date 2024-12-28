{- HLINT ignore "Redundant do" -}

module Blaze.Persist.DbSpec where

import Blaze.Prelude

import qualified Blaze.Persist.Db as Db

import Blaze.CallGraph (getCallGraph)
import Blaze.Import.Binary (openBinary)
import Blaze.Import.CallGraph (getFunctions)
import Blaze.Import.Source.Ghidra (GhidraImporter)
import qualified Blaze.Types.Graph as G

import qualified Data.HashSet as HashSet
import System.IO.Temp (withSystemTempFile)

import Test.Hspec


spec :: Spec
spec = describe "Blaze.Persist.Db" $ do
  context "CallGraph" $ do
    (imp :: GhidraImporter) <- fmap unsafeFromRight . runIO $ openBinary "res/test_bins/Dive_Logger/Dive_Logger.gzf"
    funcs <- runIO $ getFunctions imp
    cg <- runIO $ getCallGraph imp funcs
    mCg' <- runIO $ withSystemTempFile "dive_callgraph.blaze" $ \fp _ -> do
      conn <- Db.init fp
      Db.insertCallGraph conn cg
      Db.loadCallGraph conn

    let reduceGraph g = ( sort . fmap (view #name) . HashSet.toList $ G.nodes g
                        , sort $ G.edges g
                        )

    it "should store and load CallGraph" $ do
      reduceGraph <$> mCg' `shouldBe` Just (reduceGraph cg)
