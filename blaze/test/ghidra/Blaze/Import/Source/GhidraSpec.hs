{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{- HLINT ignore "Redundant do" -}

module Blaze.Import.Source.GhidraSpec where

import Blaze.CallGraph (getCallGraph)
import Blaze.Function (Function)
import Blaze.Import.Binary (BinaryImporter (getEnd, getStart, openBinary, rebaseBinary))
import Blaze.Import.CallGraph (CallGraphImporter (getCallSites, getFunctions))
import Blaze.Import.Source.Ghidra (GhidraImporter)
import qualified Blaze.Import.Source.Ghidra as G
import Blaze.Prelude hiding (Symbol)
import qualified Blaze.Types.Graph as Graph

import qualified Data.HashSet as HashSet
import Test.Hspec

diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

findFunc :: Text -> [Function] -> Maybe Function
findFunc funcName = find ((== funcName) . (^. #name))

spec :: Spec
spec = describe "Blaze.Import.Source.Ghidra" $ do
  context "binary" $ do
    (importer :: GhidraImporter) <- unsafeFromRight <$> runIO (openBinary diveBin)
    runIO $ putText "got binary ok"
    start <- runIO $ getStart importer
    end <- runIO $ getEnd importer

    it "should get start offset" $ do
      start `shouldBe` 0x8048000

    it "should get end offset" $ do
      end `shouldBe` 0x5ef

    rebasedImporter <- runIO $ rebaseBinary importer 0x10000000
    rebasedStart <- runIO $ getStart rebasedImporter

    it "should rebase binary" $ do
      rebasedStart `shouldBe` 0x10000000

  context "Importing call graphs" $ do
    importer <- runIO $ G.getImporter diveBin
    funcs <- fmap sort . runIO $ getFunctions importer
    it "should import all functions" $ do
      length funcs `shouldBe` 106

    let changeDiveFunc = fromJust $ findFunc "cgc_ChangeDive" funcs
    changeDiveCalls <- runIO $ getCallSites importer changeDiveFunc
    let printfFunc = fromJust $ findFunc "cgc_printf" funcs
    printfCalls <- runIO $ getCallSites importer printfFunc
    it "should import call sites" $ do
      length changeDiveCalls `shouldBe` 3
      length printfCalls `shouldBe` 35

    it "should get a call graph" $ do
      (sort . HashSet.toList . Graph.nodes <$> getCallGraph importer funcs) `shouldReturn` funcs
