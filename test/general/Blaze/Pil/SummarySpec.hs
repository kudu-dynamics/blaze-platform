module Blaze.Pil.SummarySpec where

import Blaze.Prelude
import qualified Blaze.Pil.Construct as C
import Blaze.Types.Pil.Summary
import Blaze.Pil.Summary (extractCapabilities)
import Test.Hspec


spec :: Spec
spec = do
  describe "mkEffect" $ do
    pure ()

  describe "extractCapabilities" $ do
    let inPtrVar = C.pilVar 8 "inptr"
        inPtr = C.var' inPtrVar 8
        outPtrVar = C.pilVar 8 "outptr"
        outPtr = C.var' outPtrVar 8

        inputVars = [inPtrVar, outPtrVar]
        garbageRet = C.const 1492 8

        tmpVar = C.pilVar 8 "tmp"
        tmp = C.var' tmpVar 8

        inGlobal = 0xcafebabedeadbeef
        outGlobal = 0xdeadbeefcafebabe
        loadGlobalStmt = C.def' tmpVar $ C.load (C.const inGlobal 8) 8
        storeGlobalStmt = C.store (C.const outGlobal 8) tmp

        loadPtrStmt = C.def' tmpVar $ C.load inPtr 8
        storePtrStmt = C.store outPtr tmp

        pureVal = C.const 384 8
        storeGlobalPureStmt = C.store (C.const outGlobal 8) pureVal
        storePtrPureStmt = C.store outPtr pureVal
        setTmpPureStmt = C.def' tmpVar pureVal

        retStmt = C.ret tmp

        garbageStmts =
          [ C.def "garbage1" $ C.add inPtr outPtr 8
          , C.store (C.var "garbage1" 8) tmp
          ]

    it "finds copy from global to other global" $ do
      extractCapabilities inputVars [garbageRet] ([loadGlobalStmt, storeGlobalStmt] <> garbageStmts)
        `shouldBe`
        [ CopyCapability (ConcreteOutputLocation outGlobal) (ConcreteInputLocation inGlobal) ]

    it "finds copy from global to OUT pointer" $ do
      extractCapabilities inputVars [garbageRet] ([loadGlobalStmt, storePtrStmt] <> garbageStmts)
        `shouldBe`
        [ CopyCapability (SymbolicOutputLocation outPtrVar) (ConcreteInputLocation inGlobal) ]

    it "finds copy from global to return value" $ do
      extractCapabilities inputVars [tmp, garbageRet] ([loadGlobalStmt, retStmt] <> garbageStmts)
        `shouldBe`
        [ CopyCapability Returned (ConcreteInputLocation inGlobal) ]

    it "ignore copy from global to non-returned value" $ do
      extractCapabilities inputVars [garbageRet] ([loadGlobalStmt, retStmt] <> garbageStmts)
        `shouldBe`
        [ ]

    it "finds copy from IN pointer to global" $ do
      extractCapabilities inputVars [garbageRet] ([loadPtrStmt, storeGlobalStmt] <> garbageStmts)
        `shouldBe`
        [ CopyCapability (ConcreteOutputLocation outGlobal) (SymbolicInputLocation inPtrVar) ]

    it "finds copy from IN pointer to OUT pointer" $ do
      extractCapabilities inputVars [garbageRet] ([loadPtrStmt, storePtrStmt] <> garbageStmts)
        `shouldBe`
        [ CopyCapability (SymbolicOutputLocation outPtrVar) (SymbolicInputLocation inPtrVar) ]

    it "finds copy from IN pointer to return value" $ do
      extractCapabilities inputVars [tmp, garbageRet] ([loadPtrStmt, retStmt] <> garbageStmts)
        `shouldBe`
        [ CopyCapability Returned (SymbolicInputLocation inPtrVar) ]

    it "ignores copy from IN pointer to non-returned value" $ do
      extractCapabilities inputVars [garbageRet] ([loadPtrStmt, retStmt] <> garbageStmts)
        `shouldBe`
        []

    it "finds copy from pure expression to global" $ do
      extractCapabilities inputVars [garbageRet] ([storeGlobalPureStmt] <> garbageStmts)
        `shouldBe`
        [ CopyCapability (ConcreteOutputLocation outGlobal) (PureExpression pureVal) ]

    it "finds copy from pure expression to OUT pointer" $ do
      extractCapabilities inputVars [garbageRet] ([storePtrPureStmt] <> garbageStmts)
        `shouldBe`
        [ CopyCapability (SymbolicOutputLocation outPtrVar) (PureExpression pureVal) ]

    it "ignores copy from pure expression to return value" $ do
      extractCapabilities inputVars [garbageRet] ([setTmpPureStmt, retStmt] <> garbageStmts)
        `shouldBe`
        []
      extractCapabilities inputVars [tmp, garbageRet] ([setTmpPureStmt, retStmt] <> garbageStmts)
        `shouldBe`
        []

  describe "fromStmts" $ do
    pure ()

  describe "removeKilledWrites" $ do
    pure ()
