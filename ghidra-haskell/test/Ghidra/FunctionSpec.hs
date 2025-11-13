module Ghidra.FunctionSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import Ghidra.Types (Function)
import Ghidra.Core
import qualified Data.BinaryAnalysis as BA

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
  let prg = gs ^. #program

  context "getFunctions" $ do
    funcs <- runIO . runGhidraOrError $ Function.getFunctions prg
    it "should get all functions for a1 binary" $ do
      length funcs `shouldBe` 37

    funcs' <- runIO . runGhidraOrError $ do
      let opts = Function.defaultGetFunctionsOptions
                 & #includeLocalFuncs .~ True
                 & #includeExternalFuncs .~ False
                 & #excludeDefaultFuncs .~ True
                 & #excludeThunks .~ False
      Function.getFunctions' opts prg
    it "should accept options when getting all functions" $ do
      length funcs' `shouldBe` 29

  context "fromAddr" $ do
    let faddr = 0x13ad
    mfunc <- runIO . runGhidraOrError $ do
      faddr' <- State.mkAddressBased prg faddr
      Function.fromAddr prg faddr'
    it "should find func by address" $ do
      void mfunc `shouldBe` Just ()

  context "getHighFunction" $ do
    let faddr = 0x13ad
    (fname1, fname2) <- runIO . runGhidraOrError $ do
      faddr' <- State.mkAddressBased prg faddr
      (Just func) <- Function.fromAddr prg faddr'
      hfunc <- Function.getHighFunction gs func
      -- Don't really know how else to easily check that HighFunc is valid
      func' :: Function <- Function.getLowFunction hfunc
      (,) <$> Function.getName func <*> Function.getName func'

    it "should get high function" $ do
      fname1 `shouldBe` fname2

  context "getParams" $ do
    it "should find params for high func" $ do
      let faddr = 0x13ad
      params <- runGhidraOrError $ do
        faddr' <- State.mkAddressBased prg faddr
        (Just func) <- Function.fromAddr prg faddr'
        hfunc <- Function.getHighFunction gs func
        Function.getHighParams hfunc
      let params' = sortOn fst $ fmap (\p -> (p ^. #ordinalIndex, p ^. #name)) params
      params' `shouldBe`
        [ (0, "param_1")
        , (1, "param_2")
        , (2, "param_3")
        , (3, "param_4")
        ]

  context "Thunks" $ do
    let putsThunkAddr = 0x1030
    putsThunkFunc <- fmap fromJust . runIO . runGhidraOrError $ do
      faddr <- State.mkAddressBased prg putsThunkAddr
      Function.fromAddr prg faddr
      
    putsThunkIsThunk <- runIO . runGhidraOrError $ Function.isThunk putsThunkFunc

    it "should identify puts as a thunk" $
      putsThunkIsThunk `shouldBe` True

    thunkedFuncDest <- runIO . runGhidraOrError $ Function.unsafeGetThunkedFunction True putsThunkFunc

    thunkedFuncDestIsExternal <- runIO . runGhidraOrError $ Function.isExternal thunkedFuncDest

    it "should identify thunk dest as external" $
      thunkedFuncDestIsExternal `shouldBe` True

    bname <- runIO . runGhidraOrError $ Function.getName thunkedFuncDest
 
    it "should identify dest of dest of puts thunk's thunk as not a thunk" $
      bname `shouldBe` "puts"

  context "externs" $ do
    let putsThunkAddr = 0x1030
    
    putsExternFunc <- runIO . runGhidraOrError $ do
      faddr <- State.mkAddressBased prg putsThunkAddr
      putsThunk <- fromJust <$> Function.fromAddr prg faddr
      Function.resolveThunk putsThunk
      
    putsIsExtern <- runIO . runGhidraOrError $ Function.isExternal putsExternFunc

    it "should identify resolved 'puts' as an extern" $
      putsIsExtern `shouldBe` True

    mPutsLibraryName <- runIO . runGhidraOrError $ do
      putsExLoc <- Function.getExternalLocation putsExternFunc
      Function.getLibraryName putsExLoc

    it "should sort-of know 'puts' library name" $
      mPutsLibraryName `shouldBe` Just "<EXTERNAL>"

    putsAddress <- runIO . runGhidraOrError $ Function.getAddress putsExternFunc

    it "should know that 'puts' address space is external" $
      putsAddress ^. #space . #name  `shouldBe` BA.EXTERNAL

    putsParams <- runIO . runGhidraOrError $ Function.getLowParams putsExternFunc

    it "should know that 'puts' takes one param" $
      length putsParams `shouldBe` 1

    it "should know the name of the first param of 'puts'" $
      (view #name <$> headMay putsParams) `shouldBe` Just "__s"

    mfunc <- runIO . runGhidraOrError $ do
      addr <- State.mkExternalAddress prg $ putsAddress ^. #offset
      Function.getFunctionAt prg addr

    it "should be able to use getFunctionAt to refind extern at extern's address" $ do
      isJust mfunc `shouldBe` True

    funcName <- runIO . runGhidraOrError $ do
      Function.getName (fromJust mfunc)

    it "should have found the puts extern function with getFunctionAt" $ do
      funcName `shouldBe` "puts"
