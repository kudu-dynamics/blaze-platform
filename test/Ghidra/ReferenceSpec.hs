module Ghidra.ReferenceSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import qualified Ghidra.Reference as Ref
import qualified Ghidra.Types as J
import qualified Ghidra.Address as Addr
import Ghidra.Core
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1"

spec :: Spec
spec = describe "Ghidra.Reference" $ do
  gs <- runIO . runGhidra $ do
    gs <- State.openDatabase_ diveBin >>! State.analyze
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

  context "toFuncReference" $ do
    let cgc_AddDive_addr = 0x804c7d0
    refs <- runIO . runGhidra $ do
      addr <- State.mkAddress gs cgc_AddDive_addr
      mfunc <- Function.fromAddr gs addr
      case mfunc of
        Nothing -> error "Couldn't find cgc_AddDive."
        Just func -> do
          funcStart <- J.toAddr func
          refs <- Ref.getReferencesTo gs funcStart
          catMaybes <$> traverse (Ref.toFuncReference gs) refs
    let refs' = sort $ fmap (\x -> ( x ^. #caller . #startAddress . #offset
                                   , x ^. #callee . #startAddress . #offset ))
                       refs
        expected = [(134533568,134531024),(134533648,134531024)]
    it "should get func references" $ do
      refs' `shouldBe` expected

    
