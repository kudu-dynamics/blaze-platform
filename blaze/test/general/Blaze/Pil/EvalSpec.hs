module Blaze.Pil.EvalSpec where

import Blaze.Prelude hiding
  ( const,
    group,
    sym,
  )
-- import Blaze.Types.Pil
import Blaze.Pil.Eval
import qualified Blaze.Pil.Construct as C

import Test.Hspec


spec :: Spec
spec = describe "Blaze.Pil.Analysis.Eval" $ do
  describe "evalPilArithmeticExpr" $ do
    it "should return a const unchanged" $ do
      let expr = C.const 888 4
          expected = Just expr
      evalPilArithmeticExpr expr `shouldBe` expected

    it "should return a float const unchanged" $ do
      let expr = C.fconst 888.0 4
          expected = Just expr
      evalPilArithmeticExpr expr `shouldBe` expected

    it "should return a pointer const unchanged" $ do
      let expr = C.constPtr 888 4
          expected = Just expr
      evalPilArithmeticExpr expr `shouldBe` expected

    it "should add two 64 bit consts" $ do
      let expr = C.add (C.const 100 8) (C.const 200 8) 8
          expected = Just $ C.const 300 8
      evalPilArithmeticExpr expr `shouldBe` expected

    it "should add const ptr and const" $ do
      let expr = C.add (C.constPtr 100 8) (C.const 200 8) 8
          expected = Just $ C.constPtr 300 8
      evalPilArithmeticExpr expr `shouldBe` expected

    it "should add const and const ptr" $ do
      let expr = C.add (C.const 100 8) (C.constPtr 200 8) 8
          expected = Just $ C.constPtr 300 8
      evalPilArithmeticExpr expr `shouldBe` expected

    it "should properly evalulate overflow of int op" $ do
      let expr = C.add (C.const 127 1) (C.const 5 1) 1
          expected = Just $ C.const (-124) 1
      evalPilArithmeticExpr expr `shouldBe` expected
