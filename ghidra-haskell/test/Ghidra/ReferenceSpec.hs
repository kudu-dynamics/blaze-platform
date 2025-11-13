module Ghidra.ReferenceSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import qualified Ghidra.Reference as Ref
import Ghidra.Core
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1.gzf"

spec :: Spec
spec = describe "Ghidra.Reference" $ do
  gs <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ diveBin >>! State.analyze
    return gs
  let prg = gs ^. #program

  context "getReferencesTo" $ do
    let cgc_printf_addr = 0x804c6e0
    refs <- runIO . runGhidraOrError $ do
      addr <- State.mkAddress_ prg cgc_printf_addr
      mfunc <- Function.fromAddr prg addr
      case mfunc of
        Nothing -> error "Couldn't find cgc_printf."
        Just func -> do
          -- startAddr <- J.toAddr func
          Ref.getReferencesTo prg func -- startAddr
    it "should get references to func" $ do
      length refs `shouldBe` 37

  context "getFunctionRefs" $ do
    let cgc_AddDive_addr = 0x804c7d0
    refs <- runIO . runGhidraOrError $ do
      addr <- State.mkAddress_ prg cgc_AddDive_addr
      mfunc <- Function.fromAddr prg addr
      case mfunc of
        Nothing -> error "Couldn't find cgc_AddDive."
        Just func -> Ref.getFunctionRefs prg func
    let refs' = sort $ fmap (\x -> ( x ^. #caller . #startAddress . #offset
                                   , x ^. #callee . #startAddress . #offset ))
                       refs
        expected = [(134533568,134531024),(134533648,134531024)]
    it "should get func references" $ do
      refs' `shouldBe` expected
