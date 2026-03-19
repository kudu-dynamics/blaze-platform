module Blaze.Import.Source.Ghidra.PilSpec where

import Blaze.Prelude

import Blaze.Import.Source.Ghidra.Pil (resolveImmediateString)
import qualified Blaze.Pil.Construct as C
import qualified Data.HashMap.Strict as HashMap

import Test.Hspec


-- | A strings map with entries at both low (bogus) and high (real) addresses.
testStringsMap :: HashMap Address Text
testStringsMap = HashMap.fromList
  [ (intToAddr 0x0, "ELF")          -- Bogus: ELF magic bytes
  , (intToAddr 0x1, "LF")           -- Bogus: ELF header artifact
  , (intToAddr 0x50, "short")       -- Bogus: still in ELF header region
  , (intToAddr 0x408cec, "Usage: %s [options]\n")  -- Real format string
  , (intToAddr 0x408d0c, "-s %s")   -- Real format string
  , (intToAddr 0x100, "boundary")   -- Edge case: exactly at threshold
  ]

spec :: Spec
spec = describe "Blaze.Import.Source.Ghidra.Pil" $ do
  context "resolveImmediateString" $ do
    it "should resolve a VImmediate to a string when address is in map and >= 0x100" $ do
      resolveImmediateString testStringsMap 0x408cec 4
        `shouldBe` C.constStr "Usage: %s [options]\n" 4

    it "should resolve at the threshold address (0x100)" $ do
      resolveImmediateString testStringsMap 0x100 4
        `shouldBe` C.constStr "boundary" 4

    it "should NOT resolve a VImmediate when address is below 0x100 even if in map" $ do
      resolveImmediateString testStringsMap 0x0 4
        `shouldBe` C.const 0x0 4

    it "should NOT resolve address 1 (common small constant)" $ do
      resolveImmediateString testStringsMap 0x1 4
        `shouldBe` C.const 0x1 4

    it "should NOT resolve address 0x50 (still below threshold)" $ do
      resolveImmediateString testStringsMap 0x50 4
        `shouldBe` C.const 0x50 4

    it "should NOT resolve when address is not in the strings map" $ do
      resolveImmediateString testStringsMap 0x999999 4
        `shouldBe` C.const 0x999999 4

    it "should NOT resolve a small constant not in the map" $ do
      resolveImmediateString testStringsMap 0x3 4
        `shouldBe` C.const 0x3 4

    it "should preserve the operand size in the resolved expression" $ do
      resolveImmediateString testStringsMap 0x408d0c 8
        `shouldBe` C.constStr "-s %s" 8
