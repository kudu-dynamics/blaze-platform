module Ghidra.DataTypesSpec where

import Test.Hspec
import Ghidra.Prelude hiding (DataType)
import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import Ghidra.Core
import Ghidra.Clang (ClangAST, ClangNode)

testBin2 :: FilePath
testBin2 = "res/test_bins/decomp-test/decomp-test5.gzf"

testStrippedBin :: FilePath
testStrippedBin = "res/test_bins/decomp-test/decomp-test-stripped.gzf"

largeTestBin :: FilePath
largeTestBin = "res/test_bins/decomp-test/mail-binary.gzf"

{- To figure out the base jaddr pointers:
Window > Memory-map > Set image base (the house icon) > set iamge base to 0 > find the function you want-}

{- The testing methadology is very similar to the ClangDecomp one, that is to encounter each type of DataType and make sure we can
   convert it into the Haskell version. This current version is a starting off point and we can add more binaries to test, but I thought
   might as well piggy-back on the work from the ClangDecomp tests -}

-- hacky, but we can change it if we are adding more tests
getBinGhidraState :: IO (Ghidra State.GhidraState, Ghidra State.GhidraState)
getBinGhidraState = do
  let gs1 = State.openDatabase_ testBin2 >>! State.analyze
  let gs2 = State.openDatabase_ largeTestBin >>! State.analyze
  return (gs1,gs2)

getClangASTFromState :: Ghidra State.GhidraState -> Int64 -> Ghidra (ClangAST ClangNode) 
getClangASTFromState gs addr = do
  gs' <- gs
  jaddr' <- State.mkAddressBased gs' addr
  (Just jfunc) <- Function.fromAddr gs' jaddr'
  clangAST <- Function.getClangAST gs' jfunc
  return clangAST

spec :: Spec
spec = beforeAll getBinGhidraState . describe "Ghidra.DataTypes" $ do

  -- We print out the AST to make sure we are getting certain datatypes
  -- this is just to see how things are printed out, so it's commented out unless needed 
   {-
  let jaddr' = 0x0de4
  clangAST' <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ testBin2 >>! State.analyze
    jaddr'' <- State.mkAddressBased gs jaddr'
    (Just jfunc) <- Function.fromAddr gs jaddr''
    clangAST'' <- Function.getClangAST gs jfunc
    return clangAST''
  let clangASTStr' :: String = show clangAST'
  _ <- runIO $ pprint clangASTStr'
   -}
  
  
  -- stripped symbols; this is just to see how things are printed out, so it's commented out unless needed
  {- 
  let jaddrs = [0x0de4, 0x04b0, 0x0520, 0x0630, 0x0934, 0x095c, 0x0984, 0x09bc, 0x09f8, 0x0af8, 0x0b84, 0x0bf4, 0x0c28, 0x0cbc, 0x0d50]
  let idx = 0
  clangAST' <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ testStrippedBin >>! State.analyze
    jaddr'' <- State.mkAddressBased gs (jaddrs !! idx)
    (Just jfunc) <- Function.fromAddr gs jaddr''
    clangAST'' <- Function.getClangAST gs jfunc
    return clangAST''
  let clangASTStr'' :: String = show clangAST'
  _ <- runIO $ pprint clangASTStr''
  -} 

  context "DataTypes General Test" $ do
    
    it "Main Func Test" $ \(gs,_) -> do
      let jaddr = 0x0de4
      clangASTMain <- runGhidraOrError $ getClangASTFromState gs jaddr
      null (show clangASTMain :: String) `shouldBe` False
    
    it "Field Func Test" $ \(gs,_) -> do
      let jaddr = 0x04b0
      clangASTField <- runGhidraOrError $ getClangASTFromState gs jaddr
      null (show clangASTField :: String) `shouldBe` False
 
    it "Switch 1 Func Test" $ \(gs,_) -> do
      let jaddr = 0x0520
      clangASTSwitch <- runGhidraOrError $ getClangASTFromState gs jaddr
      null (show clangASTSwitch :: String) `shouldBe` False
    
    it "General 1 Func Test" $ \(gs,_) -> do
      let jaddr = 0x0934
      clangASTGen1 <- runGhidraOrError $ getClangASTFromState gs jaddr
      null (show clangASTGen1 :: String) `shouldBe` False

    it "Sum Func Test" $ \(gs,_) -> do
      let jaddr = 0x095c
      clangASTSum <- runGhidraOrError $ getClangASTFromState gs jaddr
      null (show clangASTSum :: String) `shouldBe` False
    
    it "General 2 Func Test" $ \(gs,_) -> do
      let jaddr = 0x0984
      clangASTGen2 <- runGhidraOrError $ getClangASTFromState gs jaddr
      null (show clangASTGen2 :: String) `shouldBe` False
    
    it "Switch 2 Func Test" $ \(gs,_) -> do
      let jaddr = 0x0630
      clangASTSwitch2 <- runGhidraOrError $ getClangASTFromState gs jaddr
      null (show clangASTSwitch2 :: String) `shouldBe` False

    it "Sort Array Test" $ \(gs,_) -> do
      let jaddr = 0x09f8
      clangASTSortArr <- runGhidraOrError $ getClangASTFromState gs jaddr
      null (show clangASTSortArr :: String) `shouldBe` False
    
    it "Print String Test" $ \(gs,_) -> do
      let jaddr = 0x0bf4
      clangASTPrintStr <- runGhidraOrError $ getClangASTFromState gs jaddr
      null (show clangASTPrintStr :: String) `shouldBe` False
    
    it "Large Binary Func Test" $ \(_,gs) -> do
      let jaddr = 0xcfe0
      clangASTMail <- runGhidraOrError $ getClangASTFromState gs jaddr
      null (show clangASTMail :: String) `shouldBe` False
 

