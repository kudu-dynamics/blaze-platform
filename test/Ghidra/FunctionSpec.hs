module Ghidra.FunctionSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import Ghidra.Types (Function)
import Ghidra.Core
import qualified Language.Java as Java
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1.gzf"

spec :: Spec
spec = describe "Ghidra.Function" $ do
  gs <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ a1Bin >>! State.analyze
    return gs
  
  context "getFunctions" $ do
    funcs <- runIO . runGhidraOrError $ Function.getFunctions gs
    it "should get all functions for a1 binary" $ do
      length funcs `shouldBe` 37

    funcs' <- runIO . runGhidraOrError $ do
      let opts = Function.defaultGetFunctionsOptions
                 & #includeLocalFuncs .~ True
                 & #includeExternalFuncs .~ False
                 & #excludeDefaultFuncs .~ True
                 & #excludeThunks .~ False
      Function.getFunctions' opts gs
    it "should accept options when getting all functions" $ do
      length funcs' `shouldBe` 29

  context "fromAddr" $ do
    let faddr = 0x13ad
    mfunc <- runIO . runGhidraOrError $ do
      faddr' <- State.mkAddressBased gs faddr
      Function.fromAddr gs faddr'
    it "should find func by address" $ do
      void mfunc `shouldBe` Just ()

  context "getHighFunction" $ do
    let faddr = 0x13ad
    (fname1, fname2) <- runIO . runGhidraOrError $ do
      faddr' <- State.mkAddressBased gs faddr
      (Just func) <- Function.fromAddr gs faddr'
      hfunc <- Function.getHighFunction gs func
      -- Don't really know how else to easily check that HighFunc is valid
      func' :: Function <- Java.call hfunc "getFunction"
      (,) <$> Function.getName func <*> Function.getName func'
      
    it "should get high function" $ do
      fname1 `shouldBe` fname2

  context "getParams" $ do
    let faddr = 0x13ad
    params <- runIO . runGhidraOrError $ do
      faddr' <- State.mkAddressBased gs faddr
      (Just func) <- Function.fromAddr gs faddr'
      hfunc <- Function.getHighFunction gs func
      func' <- Java.call hfunc "getFunction"
      Function.getParams func'
    it "should find params for high func" $ do
      params `shouldBe` []

