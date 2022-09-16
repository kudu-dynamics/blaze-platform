module Ghidra.ReferenceSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import qualified Ghidra.Reference as Ref
import Ghidra.Core
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1"

spec :: Spec
spec = describe "Ghidra.Reference" $ do
  gs <- runIO . runGhidra $ do
    gs <- State.openDatabase diveBin >>= State.analyze
    -- b <- isNil' $ gs ^. #unGhidraState
    -- when b $ error "Couldn't open diveBin"
    return gs
  
  context "getReferencesTo" $ do
    let cgc_printf_addr = 0x804c6e0
    refs <- runIO . runGhidra $ do
      addr <- State.mkAddress gs cgc_printf_addr
      mfunc <- Function.fromAddr gs addr
      case mfunc of
        Nothing -> error "Couldn't find cgc_printf."
        Just func -> do
          -- addr <- Program.mkAddress prg cgc_printf_addr
          Ref.getReferencesTo gs func
    it "should get references to func" $ do
      length refs `shouldBe` 37

