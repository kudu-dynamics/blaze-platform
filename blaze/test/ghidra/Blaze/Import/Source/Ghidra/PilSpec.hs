module Blaze.Import.Source.Ghidra.PilSpec where

import Blaze.Prelude hiding (const)

import Blaze.Import.CallGraph (CallGraphImporter (getFunction))
import Blaze.Import.Pil (PilImporter (getFuncStatements))
import qualified Blaze.Import.Source.Ghidra as G

import Test.Hspec

diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

spec :: Spec
spec = describe "Blaze.Import.Source.Ghidra.Pil" $ do
  context "Import Function" $ do
    importer <- runIO $ G.getImporter diveBin
    mFunc <- runIO $ getFunction importer 0x804d670 -- cgc_SetParam function
    it "should import a function by address" $ do
      mFunc ^? _Just . #name `shouldBe` Just "cgc_SetParam"

    let func = fromJust mFunc
    stmts <- runIO $ getFuncStatements importer func 0

    -- stmt0: zf_1#1@10 = eax_4#1@10 == 0x0
    -- stmt1: var_14#4@10 = var_14#2@10
    -- stmt2: unique_1d00#1@10 = esp_4#1@10 + offset 0xffffffac
    -- stmt3: unique_1d00#2@10 = esp_4#1@10 + offset 0xffffffac
    let stmt0 = stmts !! 5
    let stmt1 = stmts !! 12
    let stmt2 = stmts !! 15
    let stmt3 = stmts !! 21

    let pv0 = stmt0 ^? _Ctor @"Def" . #value . #op . _Ctor @"CMP_E" . #left . #op . _Ctor @"VAR" . #src
    let pv1_l = stmt1 ^? _Ctor @"Def" . #var
    let pv1_r = stmt1 ^? _Ctor @"Def" . #value . #op . _Ctor @"VAR" . #src
    let pv2 = stmt2 ^? _Ctor @"Def" . #var
    let pv3 = stmt3 ^? _Ctor @"Def" . #var

    it "should use register names for symbols" $ do
      pv0 ^. _Just . #symbol `shouldBe` "eax_4#1"

    it "should have separate number labels for assignments to the same stack var" $ do
      pv1_l ^. _Just . #symbol `shouldBe` "var_14#4"
      pv1_r ^. _Just . #symbol `shouldBe` "var_14#2"

    it "should have separate number labels for assignments to the same unique var" $ do
      pv2 ^. _Just . #symbol `shouldBe` "unique_1d00#1"
      pv3 ^. _Just . #symbol `shouldBe` "unique_1d00#2"
