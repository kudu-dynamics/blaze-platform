{- HLINT ignore "Evaluate" -}

module Flint.SMTishSpec where


import Flint.Prelude hiding (and, const, not, or, until, sym, Location)

import Flint.SMTish (toSMTishExpression, showHexSMT)

import Blaze.Pil.Construct
import Blaze.Pretty (pretty')
import Blaze.Types.Function (Function(Function))
import qualified Blaze.Types.Pil as Pil

import Test.Hspec


func0 :: Function
func0 = Function Nothing "func0" (intToAddr 0x888) []

func1 :: Function
func1 = Function Nothing "func1" (intToAddr 0x999) []

func2 :: Function
func2 = Function Nothing "CGC_free" (intToAddr 0xAAA) []

spec :: Spec
spec = describe "Flint.SMTish" $ do
  context "showHexSMT" $ do
    it "should print out eight digits for a 4 bit number" $ do
      let width = 4
          val   = 0x1234 :: Word32
          expected = "0x00001234"
      showHexSMT width val `shouldBe` expected
  context "show SMTish" $ do
    it "should print out bvadd expr in prefix notation" $ do
        let expr = add (const 0x1234 4) (const 0x4567 4) 4 :: Pil.Expression
            expected = "(bvadd 0x00001234 0x00004567)"
        pretty' (toSMTishExpression expr) `shouldBe` expected
        
    it "should print out sub expressions in in prefix notation" $ do
        let expr = add (const 0x1234 4) (add (const 0x4567 4) (const 0x1 4) 4) 4 :: Pil.Expression
            expected = "(bvadd 0x00001234 (bvadd 0x00004567 0x00000001))"
        pretty' (toSMTishExpression expr) `shouldBe` expected 

    it "should print out array access" $ do
        let expr = arrayAddr (const 0x1234 4) (const 0x4567 4) 1 4 :: Pil.Expression
            expected = "(ARRAY_ADDR 0x00001234 0x00004567 1)"
        pretty' (toSMTishExpression expr) `shouldBe` expected

    it "should print CMP_NE as `(not (= ...))`" $ do
        let expr = cmpNE (const 0x1234 4) (const 0x4567 4) 4 :: Pil.Expression
            expected = "(not (= 0x00001234 0x00004567))"
        pretty' (toSMTishExpression expr) `shouldBe` expected

    it "should print out field address" $ do
        let expr = fieldAddr (const 0x1234 4) 0x20 4 :: Pil.Expression
            expected = "(FIELD_ADDR 0x00001234 0x00000020)"
        pretty' (toSMTishExpression expr) `shouldBe` expected

    it "should print out rlc expr" $ do
        let expr = Pil.Expression 4 $ Pil.RLC $ Pil.RlcOp (const 0x1234 4) (const 0x4567 4) (const 1 4)
            expected = "((_ rotate_left_carry 0x00001234) 0x00004567)"
        pretty' (toSMTishExpression expr) `shouldBe` expected

    it "should print out LOAD as DEREF" $ do
        let expr = load (const 0x1234 4) 4 :: Pil.Expression
            expected = "(DEREF 32 0x00001234)"
        pretty' (toSMTishExpression expr) `shouldBe` expected
