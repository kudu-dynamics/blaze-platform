module Blaze.Import.Source.Ghidra.XrefSpec where

import Blaze.Prelude

import Blaze.Import.Binary (getStringsMap)
import Blaze.Import.Xref (getXrefsTo, Xref)
import qualified Blaze.Import.Source.Ghidra as G

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

spec :: Spec
spec = describe "Blaze.Import.Source.Ghidra.Xref" $ do
  importer <- runIO $ G.getImporter diveBin
  smap <- runIO $ getStringsMap importer

  context "getXrefsTo" $ do
    it "should return xrefs for a string address that is referenced" $ do
      let addrsWithStrings = HashMap.keys smap
      xrefResults <- forM addrsWithStrings $ \addr -> do
        xrefs <- getXrefsTo importer addr
        return (addr, xrefs)
      let withXrefs = filter (\(_, xrefs) -> not (null xrefs)) xrefResults
      withXrefs `shouldSatisfy` (not . null)

    it "should return xrefs with valid function names" $ do
      let addrsWithStrings = HashMap.keys smap
      allXrefs <- fmap concat . forM addrsWithStrings $ \addr ->
        getXrefsTo importer addr
      let getName :: Xref -> Text
          getName x = x ^. #function . #name
      forM_ allXrefs $ \xref ->
        getName xref `shouldSatisfy` (not . Text.null)

    it "should return empty list for an address with no references" $ do
      xrefs <- getXrefsTo importer (intToAddr 0x7ffffffe)
      xrefs `shouldBe` []
