{- HLINT ignore "Evaluate" -}

module Blaze.Pil.Analysis.PathSpec where

import Blaze.Prelude hiding (const, sym)

import Blaze.Pil.Analysis.Path (expandVars, aggressiveExpand, simplifyArrayAddr)
import Blaze.Pil.Construct
import Blaze.Types.Pil (Stmt)
import qualified Blaze.Types.Pil as Pil

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

