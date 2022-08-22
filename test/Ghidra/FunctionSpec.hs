module Ghidra.FunctionSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import Ghidra.Function (Function)
import Ghidra.Core
import qualified Language.Java as Java
import Language.Clojure.Core
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1"

spec :: Spec
spec = describe "Ghidra.Function" $ do
  gs <- runIO . runGhidra $ do
    gs <- State.openDatabase a1Bin >>= State.analyze
    b <- isNil' $ gs ^. #unGhidraState
    when b $ error "Couldn't open a1"
    return gs
  
  context "getFunctions" $ do
    funcs <- runIO . runGhidra $ Function.getFunctions gs
    it "should get all functions for a1 binary" $ do
      length funcs `shouldBe` 37

    funcs' <- runIO . runGhidra $ do
      let opts = Function.defaultGetFunctionsOptions
                 & #defaults .~ Just False
                 & #external .~ Just False
      Function.getFunctions' (Just opts) gs
    it "should accept options when getting all functions" $ do
      length funcs' `shouldBe` 29

  context "fromAddr" $ do
    let faddr = 0x13ad
    mfunc <- runIO . runGhidra $ do
      faddr' <- State.mkAddressBased gs faddr
      Function.fromAddr gs faddr'
    it "should find func by address" $ do
      void mfunc `shouldBe` Just ()

  context "getHighFunction" $ do
    let faddr = 0x13ad
    (fname1, fname2) <- runIO . runGhidra $ do
      faddr' <- State.mkAddressBased gs faddr
      (Just func) <- Function.fromAddr gs faddr'
      hfunc <- Function.getHighFunction gs func
      -- Don't really know how else to easily check that HighFunc is valid
      func' :: Function <- Java.call hfunc "getFunction"
      (,) <$> Function.getName func <*> Function.getName func'
      
    it "should get high function" $ do
      fname1 `shouldBe` fname2

