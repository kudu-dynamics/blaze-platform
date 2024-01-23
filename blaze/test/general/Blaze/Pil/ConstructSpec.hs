module Blaze.Pil.ConstructSpec where

import Blaze.Prelude hiding (sym)
import Blaze.Types.Pil
import Blaze.Types.Function (Function(..))
import qualified Blaze.Pil.Construct as C

import Test.Hspec

spec :: Spec
spec = describe "Blaze.Pil.Construct" $ do
  describe "var" $ do
    it "should create a VAR expression with correct size and symbol" $ do
      pendingWith "Currently Construct.var produces an incorrectly-sized expression"
      let sym = "asdf"
          expectedSize = 13
          Expression exprSize op = C.var sym expectedSize
      exprSize `shouldBe` expectedSize
      case op ^? #_VAR . #_VarOp of
        Nothing -> expectationFailure "op was not a VAR"
        Just (PilVar varSize ctx varSym) -> do
          ctx `shouldBe` Nothing
          varSize `shouldBe` expectedSize
          varSym `shouldBe` sym

  describe "var'" $ do
    it "should create a VAR expression with correct size and symbol" $ do
      let sym = "asdf"
          varExpectedSize = 13
          exprExpectedSize = 25
          expectedCtx = Ctx (Function Nothing "qwer" 0xdeadbeef []) 17
          Expression exprSize op = C.var' (PilVar varExpectedSize (Just expectedCtx) sym) exprExpectedSize
      exprSize `shouldBe` exprExpectedSize
      case op ^? #_VAR . #_VarOp of
        Nothing -> expectationFailure "op was not a VAR"
        Just (PilVar varSize ctx varSym) -> do
          ctx `shouldBe` Just expectedCtx
          varSize `shouldBe` varExpectedSize
          varSym `shouldBe` sym
