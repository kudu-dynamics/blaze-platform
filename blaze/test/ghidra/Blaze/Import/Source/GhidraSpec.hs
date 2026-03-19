{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{- HLINT ignore "Redundant do" -}

module Blaze.Import.Source.GhidraSpec where

import Blaze.CallGraph (getCallGraph)
import Blaze.Function (Func, _name)
import Blaze.Import.Binary (BinaryImporter (getEnd, getStart, getStringsMap, openBinary, rebaseBinary))
import Blaze.Import.CallGraph (CallGraphImporter (getCallSites, getFunctions))
import Blaze.Import.Source.Ghidra (GhidraImporter)
import qualified Blaze.Import.Source.Ghidra as G
import Blaze.Prelude hiding (Symbol)
import qualified Blaze.Types.Graph as Graph

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Test.Hspec

diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

findFunc :: Text -> [Func] -> Maybe Func
findFunc funcName = find ((== funcName) . (^. _name))

spec :: Spec
spec = describe "Blaze.Import.Source.Ghidra" $ do
  context "binary" $ do
    (importer :: GhidraImporter) <- unsafeFromRight <$> runIO (openBinary diveBin)
    start <- runIO $ getStart importer
    end <- runIO $ getEnd importer

    it "should get start offset" $ do
      addrToInt start `shouldBe` 0x8048000

    it "should get end offset" $ do
      addrToInt end `shouldBe` 0x5ef

    rebasedImporter <- runIO $ rebaseBinary importer (intToAddr 0x10000000)
    rebasedStart <- runIO $ getStart rebasedImporter

    it "should rebase binary" $ do
      addrToInt rebasedStart `shouldBe` 0x10000000

    smap <- runIO $ getStringsMap importer

    it "should return a non-empty strings map" $ do
      HashMap.size smap `shouldSatisfy` (> 0)

    it "should contain string values" $ do
      HashMap.elems smap `shouldSatisfy` (not . null)

  context "Importing call graphs" $ do
    importer <- runIO $ G.getImporter diveBin
    funcs <- fmap sort . runIO $ getFunctions importer
    it "should import all functions" $ do
      length funcs `shouldBe` 94

    let changeDiveFunc = fromJust $ findFunc "cgc_ChangeDive" funcs
    changeDiveCalls <- runIO $ getCallSites importer changeDiveFunc
    let printfFunc = fromJust $ findFunc "cgc_printf" funcs
    printfCalls <- runIO $ getCallSites importer printfFunc
    it "should import call sites" $ do
      length changeDiveCalls `shouldBe` 3
      length printfCalls `shouldBe` 35

    -- Test that extern call sites have actual call instruction addresses,
    -- not the caller function's start address (bug fix)
    let cgcPowFunc = fromJust $ findFunc "cgc_pow" funcs
    cgcPowCalls <- runIO $ getCallSites importer cgcPowFunc
    let cgcRoundCalls = filter (\callSite -> callSite ^. #caller . #name == "cgc_round") cgcPowCalls
        cgcRoundAddr = 0x8049d20 :: Int64  -- cgc_round's function start address

    it "should find call sites to extern cgc_pow" $ do
      length cgcPowCalls `shouldSatisfy` (> 0)

    it "should have correct call instruction addresses for extern call sites" $ do
      -- cgc_round calls cgc_pow at 0x8049d66 and 0x8049d92, NOT at 0x8049d20
      length cgcRoundCalls `shouldBe` 2
      let callAddrs = sort $ fmap (addrToInt . view #address) cgcRoundCalls
      callAddrs `shouldBe` [0x8049d66, 0x8049d92]
      -- Verify none of the addresses are the caller's function start
      all ((/= cgcRoundAddr) . addrToInt . view #address) cgcRoundCalls `shouldBe` True

    it "should get a call graph" $ do
      (sort . HashSet.toList . Graph.nodes <$> getCallGraph importer funcs) `shouldReturn` funcs
