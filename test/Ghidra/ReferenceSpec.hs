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
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1.gzf"

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
          -- startAddr <- J.toAddr func
          Ref.getReferencesTo gs func -- startAddr
    it "should get references to func" $ do
      length refs `shouldBe` 37

  context "getFunctionRefs" $ do
    let cgc_AddDive_addr = 0x804c7d0
    refs <- runIO . runGhidra $ do
      addr <- State.mkAddress gs cgc_AddDive_addr
      mfunc <- Function.fromAddr gs addr
      case mfunc of
        Nothing -> error "Couldn't find cgc_AddDive."
        Just func -> Ref.getFunctionRefs gs func
    let refs' = sort $ fmap (\x -> ( x ^. #caller . #startAddress . #offset
                                   , x ^. #callee . #startAddress . #offset ))
                       refs
        expected = [(134533568,134531024),(134533648,134531024)]
    it "should get func references" $ do
      refs' `shouldBe` expected

    
