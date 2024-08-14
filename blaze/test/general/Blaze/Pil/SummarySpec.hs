module Blaze.Pil.SummarySpec where

import Blaze.Prelude
import qualified Blaze.Pil.Construct as C
import Blaze.Pil.Analysis (LoadExpr (LoadExpr), getVarsFromExpr)
import Blaze.Types.Pil.Summary
import Blaze.Pil.Summary (extractCapabilities, getReadsWrites, filterArgs)
import qualified Data.HashSet as HashSet
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

  describe "getReadsWrites" $ do
    let arg0 = C.var "arg0" 8
        arg1 = C.var "arg1" 8
        arg2 = C.var "arg2" 8
        arg3 = C.var "arg3" 8
        arg4 = C.var "arg4" 8

        var0 = C.var "var0" 8
        var1 = C.var "var1" 8
        var2 = C.var "var2" 8
        var3 = C.var "var3" 8

        expr0 = LoadExpr (C.load arg0 4)
        expr1 = LoadExpr (C.load var0 4)
        expr2 = LoadExpr (C.load arg1 4)
        expr3 = LoadExpr (C.load arg1 4)

        ew0 = EffectWrite (C.store arg2 var1)
        ew1 = EffectWrite (C.store var2 var1)
        ew2 = EffectWrite (C.store var3 arg3)
        ew3 = EffectWrite (C.store arg4 arg3)

        sampleCS0 = CodeSummary
              { inputVars = []
              , inputLoads = []
              , results = []
              , effects = []
              , capabilities = []
              }
        sampleCS1 = CodeSummary
              { inputVars = []
              , inputLoads = [expr0, expr1, expr2, expr3]
              , results = []
              , effects = [ew0, ew1, ew2, ew3]
              , capabilities = []
              }

        cs1Reads = foldMap getVarsFromExpr [arg0, arg1, var0]
        cs1Writes = foldMap getVarsFromExpr [arg2, arg4, var2, var3]
        cs1ArgReads = foldMap getVarsFromExpr [arg0, arg1]
        cs1ArgWrites = foldMap getVarsFromExpr [arg2, arg4]

    it "returns an empty ReadsWrites when no LoadExprs or EffectWrites are present" $ do
      getReadsWrites [sampleCS0]
        `shouldBe`
        ReadsWrites HashSet.empty HashSet.empty

    it "returns all vars present in a ReadsWrites in a normal example" $ do
      getReadsWrites [sampleCS1]
        `shouldBe`
        ReadsWrites cs1Reads cs1Writes

    it "returns all args present in a ReadsWrites in a normal example" $ do
      filterArgs (getReadsWrites [sampleCS1])
        `shouldBe`
        ReadsWrites cs1ArgReads cs1ArgWrites
