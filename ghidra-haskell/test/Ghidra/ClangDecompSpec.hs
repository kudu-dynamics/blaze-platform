
module Ghidra.ClangDecompSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import Ghidra.Core
import Test.Hspec

testBin2 :: FilePath
testBin2 = "res/test_bins/decomp-test/decomp-test2.gzf"

largeTestBin :: FilePath
largeTestBin = "res/test_bins/decomp-test/mail-binary.gzf"

{- To figure out the base jaddr pointers:
Window > Memory-map > Set image base (the house icon) > set iamge base to 0 > find the function you want-}

spec :: Spec
spec = describe "Ghidra.ClangDecomp" $ do
  let path = testBin2

  let jaddrMain = 0x3e44
  clangASTMain <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ path >>! State.analyze
    jaddr' <- State.mkAddressBased gs jaddrMain
    (Just jfunc) <- Function.fromAddr gs jaddr'
    clangAST' <- Function.getClangAST gs jfunc
    return clangAST'
  let clangASTMainStr :: String = show clangASTMain

  let jaddrField = 0x3920
  clangASTField <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ path >>! State.analyze
    jaddr' <- State.mkAddressBased gs jaddrField
    (Just jfunc) <- Function.fromAddr gs jaddr'
    clangAST' <- Function.getClangAST gs jfunc
    return clangAST'
  let clangASTFieldStr :: String = show clangASTField
  
  let jaddrSwitch = 0x3994
  clangASTSwitch <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ path >>! State.analyze
    jaddr' <- State.mkAddressBased gs jaddrSwitch
    (Just jfunc) <- Function.fromAddr gs jaddr'
    clangAST' <- Function.getClangAST gs jfunc
    return clangAST'
  let clangASTSwitchStr :: String = show clangASTSwitch

  let jaddrGenTest1 = 0x3dbc
  clangASTGen1 <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ path >>! State.analyze
    jaddr' <- State.mkAddressBased gs jaddrGenTest1
    (Just jfunc) <- Function.fromAddr gs jaddr'
    clangAST' <- Function.getClangAST gs jfunc
    return clangAST'
  let clangASTGen1Str :: String = show clangASTGen1
  
  let jaddrSum = 0x3de4
  clangASTSum <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ path >>! State.analyze
    jaddr' <- State.mkAddressBased gs jaddrSum
    (Just jfunc) <- Function.fromAddr gs jaddr'
    clangAST' <- Function.getClangAST gs jfunc
    return clangAST'
  let clangASTSumStr :: String = show clangASTSum

  let jaddrGenTest2 = 0x3e0c
  clangASTGen2 <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ path >>! State.analyze
    jaddr' <- State.mkAddressBased gs jaddrGenTest2
    (Just jfunc) <- Function.fromAddr gs jaddr'
    clangAST' <- Function.getClangAST gs jfunc
    return clangAST'
  let clangASTGen2Str :: String = show clangASTGen2

  let largeBinPath = largeTestBin
  let jaddrLargeTest = 0xcfe0
  clangASTMail <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ largeBinPath >>! State.analyze
    jaddr' <- State.mkAddressBased gs jaddrLargeTest
    (Just jfunc) <- Function.fromAddr gs jaddr'
    clangAST' <- Function.getClangAST gs jfunc
    return clangAST'
  let clangASTMailStr :: String = show clangASTMail

  let jaddrSwitch2 = 0x3a84
  clangASTSwitch2 <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ testBin2 >>! State.analyze
    jaddr' <- State.mkAddressBased gs jaddrSwitch2
    (Just jfunc) <- Function.fromAddr gs jaddr'
    clangAST' <- Function.getClangAST gs jfunc
    return clangAST'
  let clangASTSwitch2Str :: String = show clangASTSwitch2

  context "ClangAST General Test" $ do
    it "Main Func Test" $ do
        null clangASTMainStr `shouldBe` False
    it "Field Func Test" $ do
        null clangASTFieldStr `shouldBe` False
    it "Switch 1 Func Test" $ do
        null clangASTSwitchStr `shouldBe` False
    it "General 1 Func Test" $ do
        null clangASTGen1Str `shouldBe` False
    it "Sum Func Test" $ do
        null clangASTSumStr `shouldBe` False
    it "General 2 Func Test" $ do
        null clangASTGen2Str `shouldBe` False
    it "Large Binary Func Test" $ do
        null clangASTMailStr `shouldBe` False
    it "Switch 2 Func Test" $ do
        null clangASTSwitch2Str `shouldBe` False
