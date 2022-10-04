{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{- HLINT ignore "Redundant do" -}
module Blaze.IndirectCallResolutionSpec where

-- import qualified Binja.Core as Bn
-- import Blaze.IndirectCallResolution
import Blaze.Prelude
-- import qualified Blaze.Import.Source.BinaryNinja as Bni
import Test.Hspec

boxBin :: FilePath
boxBin = "res/test_bins/box_sample/box.bndb"

spec :: Spec
spec = describe "Blaze.IndirectCallResolution" $ do
  context "when avoiding" $ do
    it "placeholder" $
      True `shouldBe` True
  -- eBoxBinBv <- runIO $ Bn.getBinaryView boxBin
  -- let (Right boxBinBv) = eBoxBinBv
  -- runIO $ Bn.updateAnalysisAndWait boxBinBv

  -- context "Constructors" $ do
  --   let bni = Bni.BNImporter boxBinBv
  --   constructors <- runIO $ getConstructors bni
  --   it "should get all constructors in the binaryview" $ do
  --      length constructors `shouldBe` 2

  -- context "Indirect Calls" $ do
  --   indirectCalls <- runIO $ getIndirectCallSites boxBinBv
  --   it "should get all Indirect Calls in the binaryview" $ do
  --      length indirectCalls `shouldBe` 2

  -- context "CallGraph Paths" $ do
  --   let importer = Bni.BNImporter boxBinBv
  --   useTs <- runIO $ getIndirectCallTrees boxBinBv 
  --   dCg <- runIO $ getDirectedCg importer
  --   let edgeL = getEdgeList dCg
  --   constructors <- runIO $ getConstructors importer
  --   indirectCalls <- runIO $ getIndirectCallSites boxBinBv
  --   let edgesFromIcsToConstrs = map (edgesToCandidateConstructors (extractFuncsFromConstructors constructors) edgeL) useTs
  --   it "should get all callgraph edges from the indirect calls to their potential constructors" $ do
  --      length (head edgesFromIcsToConstrs) `shouldBe` 2
  --   let iCCcsPairs = constructorsForIndirectCalls constructors indirectCalls edgesFromIcsToConstrs
  --   it "should get all candidate constructors for each indirect call" $ do
  --      length (head iCCcsPairs) `shouldBe` 1
  --   return ()
