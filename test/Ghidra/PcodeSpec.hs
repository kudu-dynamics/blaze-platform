module Ghidra.PcodeSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import Ghidra.Function (Function)
import Ghidra.Pcode (getRawPcodeOps, getHighPcodeOps)
import Ghidra.Core
import qualified Language.Java as Java
import Language.Clojure.Core
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1"

spec :: Spec
spec = describe "Ghidra.Pcode" $ do
  gs <- runIO . runGhidra $ do
    gs <- State.openDatabase a1Bin >>= State.analyze
    b <- isNil' $ gs ^. #unGhidraState
    when b $ error "Couldn't open a1"
    return gs
  
  context "getRawPcodeOps" $ do
    let faddr = 0x13ad
    raws <- runIO . runGhidra $ do
      faddr' <- State.mkAddressBased gs faddr
      (Just func) <- Function.fromAddr gs faddr'
      getRawPcodeOps gs func
      
    it "should get raw pcode" $ do
      length raws `shouldBe` 104

  context "getHighPcodeOps" $ do
    let faddr = 0x13ad
    highs <- runIO . runGhidra $ do
      faddr' <- State.mkAddressBased gs faddr
      (Just func) <- Function.fromAddr gs faddr'
      hfunc <- Function.getHighFunction gs func
      getHighPcodeOps gs hfunc func
      
    it "should get high pcode" $ do
      length highs `shouldBe` 15

