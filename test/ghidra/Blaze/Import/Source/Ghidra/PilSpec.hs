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
    -- The statement used for the test: zf_1@0 = eax_4@0 == 0x0
    let stmt = stmts !! 5
    let pv = stmt ^? _Ctor @"Def" . #value . #op . _Ctor @"CMP_E" . #left . #op . _Ctor @"VAR" . #src

    it "should use register names for symbols" $ do
      pv ^. _Just . #symbol `shouldBe` "eax_4"
