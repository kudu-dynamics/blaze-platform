{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{- HLINT ignore "Redundant do" -}

module Blaze.Import.Source.GhidraSpec where

import Blaze.CallGraph (getCallGraph)
import Blaze.Function (Function)
import Blaze.Import.CallGraph (CallGraphImporter (getCallSites, getFunctions))
import Blaze.Prelude hiding (Symbol)
import qualified Blaze.Import.Source.Ghidra as G
import qualified Blaze.Types.Graph as Graph

import qualified Data.HashSet as HashSet
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

findFunc :: Text -> [Function] -> Maybe Function
findFunc funcName = find ((== funcName) . (^. #name))

spec :: Spec
spec = describe "Blaze.Import.Source.Ghidra" $ do
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
