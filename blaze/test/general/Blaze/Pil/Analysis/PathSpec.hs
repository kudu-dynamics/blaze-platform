{- HLINT ignore "Evaluate" -}

module Blaze.Pil.Analysis.PathSpec where

import Blaze.Prelude hiding (const, sym)

import Blaze.Pil.Analysis.Path (expandVars, aggressiveExpand, simplifyArrayAddr, promoteStackLocals)
import Blaze.Pil.Construct
import Blaze.Types.Pil (Stmt, Expression(Expression))
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Types.Function as Func
import qualified Numeric

import Test.Hspec


constStmts :: [Stmt]
constStmts =
  [ def "a" (const 42 4)
  , def "b" (var "a" 4)
  , def "c" (add (var "b" 4) (const 1 4) 4)
  ]

spec :: Spec
spec = describe "Blaze.Pil.Analysis" $ do
  describe "expandVars" $ do
    it "should do nothing for an empty list of statements" $ do
      let stmts = []
          result = expandVars stmts
          expected = []
      result `shouldBe` expected

    it "should eliminate a single assignemnt statement" $ do
      let stmts = [ def "x" (const 44 8) ]
          result = expandVars stmts
          expected = []
      result `shouldBe` expected

    it "should substitute an expression for var everywhere it appears" $ do
      let stmts = [ def "x" (add (var "y" 8) (const 1 8) 8)
                  , def "z" (var "x" 8)
                  , constraint $ cmpE (var "x" 8) (var "b" 8) 8
                  ]
          result = expandVars stmts
          expected =
            [ constraint $ cmpE (add (var "y" 8) (const 1 8) 8) (var "b" 8) 8
            ]
      result `shouldBe` expected

    it "should substitute two parallel vars" $ do
      let stmts =
            [ def "x" (add (var "y" 8) (const 1 8) 8)
            , def "z" (const 88 8)
            , constraint $ cmpE (var "x" 8) (var "z" 8) 8
            ]
          result = expandVars stmts
          expected =
            [ constraint $ cmpE (add (var "y" 8) (const 1 8) 8) (const 88 8) 8
            ]
      result `shouldBe` expected

    it "should trickle down substitutions" $ do
      let stmts =
            [ def "x" (add (var "y" 8) (const 1 8) 8)
            , def "z" (sub (var "x" 8) (const 1 88) 8)
            , constraint $ cmpE (const 777 8) (var "z" 8) 8
            ]
          result = expandVars stmts
          expected =
            [ constraint $ cmpE (const 777 8)
              (sub (add (var "y" 8) (const 1 8) 8) (const 1 88) 8)
              8
            ]
      result `shouldBe` expected

    it "should not subst vars equal to loads" $ do
      let stmts =
            [ def "y" (load (var "x" 8) 8)
            , constraint $ cmpE (const 777 8) (var "y" 8) 8
            ]
          result = expandVars stmts
          expected = stmts
      result `shouldBe` expected

    it "should not subst vars equal to calls" $ do
      let stmts =
            [ defCall "y" Pil.CallUnk [var "x" 8] 8
            , constraint $ cmpE (const 777 8) (var "y" 8) 8
            ]
          result = expandVars stmts
          expected = stmts
      result `shouldBe` expected

  context "aggressiveExpand" $ do

    {-
    it "should aggressive expand a long set of stmts without looping forever" $ do
      stmts <- readAsJSON "res/json/dive_logger_aggressive_expand_failure_path.json"
      let result = aggressiveExpand stmts
      (length result < length stmts) `shouldBe` True
    -}

    it "should use a previously used var for phi" $ do
      let stmts =
            [ defCall "y" Pil.CallUnk [var "x" 8] 8
            , defPhi 8 "z" ["m", "n", "x"]
            , ret $ var "z" 8
            ]
          expected =
            [ defCall "y" Pil.CallUnk [var "x" 8] 8
            , ret $ var "x" 8
            ]
          result = aggressiveExpand stmts
      result `shouldBe` expected

  context "simplifyArrayAddr" $ do
    it "should collapse nested ARRAY_ADDR with same stride" $ do
      -- ptr[1][1] → ptr[2]
      let ptr = var "ptr" 4
          nested = arrayAddr (arrayAddr ptr (const 1 4) 1 4) (const 1 4) 1 4
          expected = arrayAddr ptr (const 2 4) 1 4
      (simplifyArrayAddr nested :: Pil.Expression) `shouldBe` expected

    it "should collapse deeply nested ARRAY_ADDR (simulating many fputc calls)" $ do
      -- ptr[1][1][1][1][1] → ptr[5]
      let ptr = var "ptr" 4
          nested = foldr (\_ acc -> arrayAddr acc (const 1 4) 1 4) ptr [1..5 :: Int]
          expected = arrayAddr ptr (const 5 4) 1 4
      (simplifyArrayAddr nested :: Pil.Expression) `shouldBe` expected

    it "should not collapse ARRAY_ADDR with different strides" $ do
      let ptr = var "ptr" 4
          nested = arrayAddr (arrayAddr ptr (const 1 4) 1 4) (const 1 4) 2 4
      (simplifyArrayAddr nested :: Pil.Expression) `shouldBe` nested

    it "should not modify a single ARRAY_ADDR" $ do
      let ptr = var "ptr" 4
          single = arrayAddr ptr (const 3 4) 1 4
      (simplifyArrayAddr single :: Pil.Expression) `shouldBe` single

    it "should collapse nested FIELD_ADDR by adding offsets" $ do
      -- base.+8.+16 → base.+24
      let base = var "base" 4
          nested = fieldAddr (fieldAddr base 8 4) 16 4
          expected = fieldAddr base 24 4
      (simplifyArrayAddr nested :: Pil.Expression) `shouldBe` expected

    it "should collapse deeply nested FIELD_ADDR" $ do
      -- base.+4.+4.+4 → base.+12
      let base = var "base" 4
          nested = fieldAddr (fieldAddr (fieldAddr base 4 4) 4 4) 4 4
          expected = fieldAddr base 12 4
      (simplifyArrayAddr nested :: Pil.Expression) `shouldBe` expected

    it "should handle non-constant ARRAY_ADDR indices with ADD" $ do
      -- ptr[x][y] → ptr[x + y] (same stride)
      let ptr = var "ptr" 4
          x = var "x" 4
          y = var "y" 4
          nested = arrayAddr (arrayAddr ptr x 1 4) y 1 4
          expected = arrayAddr ptr (add x y 4) 1 4
      (simplifyArrayAddr nested :: Pil.Expression) `shouldBe` expected

    it "should work inside larger expressions" $ do
      -- load(ptr[1][1]) → load(ptr[2])
      let ptr = var "ptr" 4
          nested = load (arrayAddr (arrayAddr ptr (const 1 4) 1 4) (const 1 4) 1 4) 4
          expected = load (arrayAddr ptr (const 2 4) 1 4) 4
      (simplifyArrayAddr nested :: Pil.Expression) `shouldBe` expected

  context "promoteStackLocals" $ do
    let dummyFunc = Func.Function Nothing "test_func" (intToAddr 0x1000) []
        dummyCtx = Pil.Ctx dummyFunc 0 False
        stackAddr off = Expression 4 . Pil.STACK_ADDR $ Pil.StackOffset dummyCtx off
        storeStack off = store (stackAddr off)
        loadStack off = load (stackAddr off)
        -- The promoted PilVar that promoteStackLocals should create
        stackPv off ver sz = pilVar__ (getPilVarSize sz) (Just dummyCtx) ver
                               ((if off < 0 then "var_" else "arg_") <> cs (Numeric.showHex (abs off) ""))
                               (off >= 0) (Pil.StackMemory off)

    it "should promote a non-escaped stack local from Store/Load to Def/Var" $ do
      let stmts =
            [ storeStack (-0x28) (const 42 4)
            , def "a0" (loadStack (-0x28) 4)
            ]
          pv1 = stackPv (-0x28) (Just 1) (4 :: Pil.Size Expression)
          expected =
            [ def' pv1 (const 42 4)
            , def "a0" (var' pv1 4)
            ]
      promoteStackLocals stmts `shouldBe` expected

    it "should NOT promote an escaped stack local (address taken)" $ do
      let stmts =
            [ storeStack (-0x28) (const 42 4)
            , def "a0" (stackAddr (-0x28))  -- bare STACK_ADDR → escaped
            , def "a1" (loadStack (-0x28) 4)
            ]
      -- Escaped: stmts unchanged
      promoteStackLocals stmts `shouldBe` stmts

    it "should NOT promote a stack local with mixed-width accesses" $ do
      let stmts =
            [ storeStack (-0x28) (const 42 1)  -- byte store
            , def "a0" (loadStack (-0x28) 4)   -- word load
            ]
      -- Width mismatch: stmts unchanged
      promoteStackLocals stmts `shouldBe` stmts

    it "should promote non-escaped while leaving escaped untouched" $ do
      let stmts =
            [ storeStack (-0x28) (const 1 4)   -- non-escaped
            , storeStack (-0x30) (const 2 4)   -- will be escaped
            , def "a0" (stackAddr (-0x30))      -- escapes -0x30
            , def "a1" (loadStack (-0x28) 4)   -- reads non-escaped
            , def "a2" (loadStack (-0x30) 4)   -- reads escaped
            ]
          pv28_v1 = stackPv (-0x28) (Just 1) (4 :: Pil.Size Expression)
          expected =
            [ def' pv28_v1 (const 1 4)
            , storeStack (-0x30) (const 2 4)
            , def "a0" (stackAddr (-0x30))
            , def "a1" (var' pv28_v1 4)
            , def "a2" (loadStack (-0x30) 4)
            ]
      promoteStackLocals stmts `shouldBe` expected

    it "should rewrite DefPhi vars to match promoted stack locals" $ do
      -- Simulate what the lifter produces: Store (from VStack output),
      -- DefPhi (from MULTIEQUAL via phiVarNode with SSA versions),
      -- and Load (from VStack input).
      let versionedPv ver = pilVar__ (getPilVarSize (4 :: Pil.Size Expression)) (Just dummyCtx)
                              (Just ver) "var_28" False (Pil.StackMemory (-0x28))
          phiOut = versionedPv 3
          phiIn1 = versionedPv 1
          phiIn2 = versionedPv 2
          stmts =
            [ storeStack (-0x28) (const 42 4)
            , defPhi' phiOut [phiIn1, phiIn2]
            , def "a0" (loadStack (-0x28) 4)
            ]
          -- Store gets version 1. The phi and Load are rewritten to
          -- version 1 (the current version at their position).
          pv1 = stackPv (-0x28) (Just 1) (4 :: Pil.Size Expression)
          expected =
            [ def' pv1 (const 42 4)
            , defPhi' pv1 [pv1, pv1]
            , def "a0" (var' pv1 4)
            ]
      promoteStackLocals stmts `shouldBe` expected

    it "should not confuse same-offset stack locals from different contexts" $ do
      -- In an expanded path, caller and callee both have offset -0x28
      -- but different Ctx. They must be promoted independently.
      let callerFunc = Func.Function Nothing "caller" (intToAddr 0x2000) []
          calleeFunc = Func.Function Nothing "callee" (intToAddr 0x3000) []
          callerCtx = Pil.Ctx callerFunc 0 False
          calleeCtx = Pil.Ctx calleeFunc 1 False
          callerAddr off = Expression 4 . Pil.STACK_ADDR $ Pil.StackOffset callerCtx off
          calleeAddr off = Expression 4 . Pil.STACK_ADDR $ Pil.StackOffset calleeCtx off
          callerPv off ver sz = pilVar__ (getPilVarSize sz) (Just callerCtx) ver
                                 ((if off < 0 then "var_" else "arg_") <> cs (Numeric.showHex (abs off) ""))
                                 (off >= 0) (Pil.StackMemory off)
          calleePv off ver sz = pilVar__ (getPilVarSize sz) (Just calleeCtx) ver
                                 ((if off < 0 then "var_" else "arg_") <> cs (Numeric.showHex (abs off) ""))
                                 (off >= 0) (Pil.StackMemory off)
          -- Phi vars from callee's context
          calleePhiPv ver = pilVar__ (getPilVarSize (4 :: Pil.Size Expression)) (Just calleeCtx)
                              (Just ver) "var_28" False (Pil.StackMemory (-0x28))
          stmts =
            [ store (callerAddr (-0x28)) (const 1 4)          -- caller's -0x28
            , store (calleeAddr (-0x28)) (const 2 4)          -- callee's -0x28
            , defPhi' (calleePhiPv 3) [calleePhiPv 1, calleePhiPv 2]
            , def "r1" (load (callerAddr (-0x28)) 4)          -- read caller's
            , def "r2" (load (calleeAddr (-0x28)) 4)          -- read callee's
            ]
          -- Each context gets its own version counter: caller v1, callee v1
          pvCaller1 = callerPv (-0x28) (Just 1) (4 :: Pil.Size Expression)
          pvCallee1 = calleePv (-0x28) (Just 1) (4 :: Pil.Size Expression)
          expected =
            [ def' pvCaller1 (const 1 4)
            , def' pvCallee1 (const 2 4)
            , defPhi' pvCallee1 [pvCallee1, pvCallee1]
            , def "r1" (var' pvCaller1 4)
            , def "r2" (var' pvCallee1 4)
            ]
      promoteStackLocals stmts `shouldBe` expected

