{- HLINT ignore "Redundant do" -}

module Blaze.CallGraphSpec where

import qualified Binja.Core as BN
import qualified Blaze.CallGraph as Cg
import qualified Blaze.Import.CallGraph as Cgi
import Blaze.Prelude
import qualified Blaze.Types.Graph as G
import qualified Blaze.Import.Source.BinaryNinja as Bni
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.bndb"

spec :: Spec
spec = describe "Blaze.CallGraph" $ do
  context "Dive_Logger" $ do
    bv <- unsafeFromRight <$> runIO (BN.getBinaryView diveBin)
    runIO $ BN.updateAnalysisAndWait bv
    let importer = Bni.BNImporter bv
    funcs <- runIO $ Cgi.getFunctions importer
    cg <- runIO $ Cg.getCallGraph importer funcs
    it "should load a call graph with the correct number of nodes" $ do
      length (G.nodes cg) `shouldBe` 94
    it "should load a call graph with the correct number of edges" $ do
      length (G.edges cg) `shouldBe` 155
