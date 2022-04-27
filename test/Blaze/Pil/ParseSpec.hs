module Blaze.Pil.ParseSpec where

import qualified Blaze.Pil.Construct as C
import Blaze.Pil.Parse
import Blaze.Prelude
import Blaze.Pretty (PrettyShow' (PrettyShow'), Tokenizable)
import Blaze.Types.Function (Function (Function))
import Blaze.Types.Pil (Ctx (Ctx), CtxId (CtxId))
import qualified Data.Bimap as Bimap
import Data.BinaryAnalysis (Symbol (Symbol))
import Data.List (foldl1, foldr1)
import Test.Hspec
import Test.Hspec.Megaparsec (parseSatisfies, shouldFailOn, shouldParse)
import Text.Megaparsec (eof)

testp :: (Show a, Eq a) => ParserCtx -> Parser a -> Text -> a -> Expectation
testp ctx p s expected = runParser' ctx (p <* eof) s `shouldParse` expected

testpPretty :: (Tokenizable a, Eq a) => ParserCtx -> Parser a -> Text -> a -> Expectation
testpPretty ctx p s expected = runParser' ctx (PrettyShow' <$> p <* eof) s `shouldParse` PrettyShow' expected

failp :: (Show a) => ParserCtx -> Parser a -> Text -> Expectation
failp ctx p s = runParser' ctx (p <* eof) `shouldFailOn` s

failpPretty :: (Tokenizable a) => ParserCtx -> Parser a -> Text -> Expectation
failpPretty ctx p s = runParser' ctx (PrettyShow' <$> p <* eof) `shouldFailOn` s

satp :: (Eq a, Show a) => ParserCtx -> Parser a -> Text -> (a -> Bool) -> Expectation
satp ctx p s t = runParser' ctx p s `parseSatisfies` t

spec :: Spec
spec = do
  let bc = blankParserCtx
  describe "parseVar" $ do
    let varCtxTest pctx ctx fullSymbol name =
          satp pctx parseVar fullSymbol (\v -> v ^. #symbol == name && v ^. #ctx == Just ctx)
        varTest name =
          satp bc parseVar name (\v -> v ^. #symbol == name)
    it "accepts valid one character identifiers" $ do
      traverse_
        varTest
        [ "a"
        , "B"
        , "_"
        ]
    it "accepts other valid identifiers" $ do
      traverse_
        varTest
        [ "a3"
        , "a_"
        , "var_38#7"
        , "arg_0#0"
        , "__return_addr"
        , "x''"
        , "a_1#b'2$c:d.4!e~5"
        , "$a0_36#8"
        ]
    it "rejects the empty string" $ do
      failp bc parseVar ""
    it "rejects invalid identifiers" $ do
      traverse_
        (failp bc parseVar)
        [ "#"
        , "'"
        , ":"
        , "."
        , "!"
        , "~"
        , "3"
        ]
    let ctx0 =
          Ctx
            (Function (Just (Symbol "f0" "f0")) "f0" 0x4000 [])
            (CtxId 0)
        ctx1 =
          Ctx
            (Function (Just (Symbol "f1" "f1")) "f1" 0x4001 [])
            (CtxId 1)
        ctx2 =
          Ctx
            (Function (Just (Symbol "f2" "f2")) "f2" 0x4002 [])
            (CtxId 2)
        ctxs = Bimap.fromList [(0, ctx0), (1, ctx1), (2, ctx2)]
        parserCtx = ParserCtx ctxs
    it "recognizes valid context indices" $ do
      traverse_
        (\(ctx, full, name) -> varCtxTest parserCtx ctx full name)
        [ (ctx0, "asdf_3#4@0", "asdf_3#4")
        , (ctx1, "asdf_3#4@1", "asdf_3#4")
        , (ctx2, "asdf_3#4@2", "asdf_3#4")
        ]
    it "rejects invalid context indices" $ do
      failp bc parseVar "asdf_3#4@0"
      failp bc parseVar "asdf_3#4@1"
      failp parserCtx parseVar "asdf_3#4"
      failp parserCtx parseVar "asdf_3#4@3"
      failp parserCtx parseVar "asdf_3#4@4"

  describe "parseInt" $ do
    it "does not accept the empty string" $ do
      failp bc parseInt ""
    context "given binary input" $ do
      it "parses any binary digit" $ do
        testp bc parseInt "0b01" 1
        testp bc parseInt "0b10" 2
        testp bc parseInt "0b10011010100110" 9894
        testp bc parseInt "0B10011010100110" 9894
      it "parses negative numbers" $ do
        testp bc parseInt "-0b01" (-1)
        testp bc parseInt "-0b10" (-2)
        testp bc parseInt "-0b10011010100110" (-9894)
        testp bc parseInt "-0B10011010100110" (-9894)
      it "does not allow non-binary characters" $ do
        failp bc parseInt "0b2"
        failp bc parseInt "-0b2"
        failp bc parseInt "0bg"
        failp bc parseInt "0bG"
        failp bc parseInt "0b_"
    context "given octal input" $ do
      it "parses any octal digit" $ do
        testp bc parseInt "0o01234567" 0o01234567
        testp bc parseInt "0o76543210" 0o76543210
        testp bc parseInt "0o27645126371265" 0o27645126371265
        testp bc parseInt "0O27645126371265" 0o27645126371265
      it "parses negative numbers" $ do
        testp bc parseInt "-0o01234567" (-0o01234567)
        testp bc parseInt "-0o76543210" (-0o76543210)
        testp bc parseInt "-0o27645126371265" (-0o27645126371265)
        testp bc parseInt "-0O27645126371265" (-0o27645126371265)
      it "does not allow non-octal characters" $ do
        failp bc parseInt "0o8"
        failp bc parseInt "-0o8"
        failp bc parseInt "0og"
        failp bc parseInt "0oG"
        failp bc parseInt "0o_"
    context "given decimal input" $ do
      it "parses any decimal digit" $ do
        testp bc parseInt "0n1234567890" 1234567890
        testp bc parseInt "0N1234567890" 1234567890
        testp bc parseInt "1234567890" 1234567890
        testp bc parseInt "340987561033841023486" 340987561033841023486
        testp bc parseInt "0n340987561033841023486" 340987561033841023486
      it "parses negative numbers" $ do
        testp bc parseInt "-0n1234567890" (-1234567890)
        testp bc parseInt "-0N1234567890" (-1234567890)
        testp bc parseInt "-1234567890" (-1234567890)
        testp bc parseInt "-340987561033841023486" (-340987561033841023486)
        testp bc parseInt "-0n340987561033841023486" (-340987561033841023486)
      it "does not allow non-decimal characters" $ do
        failp bc parseInt "0na"
        failp bc parseInt "a"
        failp bc parseInt "-0NA"
        failp bc parseInt "-0nA"
        failp bc parseInt "-0na"
        failp bc parseInt "_"
    context "given hexadecimal input" $ do
      it "parses any hexadecimal digit" $ do
        testp bc parseInt "0x1234567890abcdefABCDEF" 0x1234567890abcdefABCDEF
        testp bc parseInt "0X1234567890abcdefABCDEF" 0x1234567890abcdefABCDEF
        testp bc parseInt "0x81913fabd91Dc02567E" 0x81913fabd91Dc02567E
      it "parses negative numbers" $ do
        testp bc parseInt "-0x1234567890abcdefABCDEF" (-0x1234567890abcdefABCDEF)
        testp bc parseInt "-0X1234567890abcdefABCDEF" (-0x1234567890abcdefABCDEF)
        testp bc parseInt "-0x81913fabd91Dc02567E" (-0x81913fabd91Dc02567E)
      it "does not allow non-hexadecimal characters" $ do
        failp bc parseInt "-0xg"
        failp bc parseInt "0xg"
        failp bc parseInt "-0xG"
        failp bc parseInt "-0x_"

  describe "parseExpr" $ do
    it "respects associativity" $ do
      testpPretty bc parseExpr "1 + 2 + 3 + 4" $ foldl1 (\x y -> C.add x y 8) (flip C.const 8 <$> [1, 2, 3, 4])
      testpPretty bc parseExpr "1 - 2 - 3 - 4" $ foldl1 (\x y -> C.sub x y 8) (flip C.const 8 <$> [1, 2, 3, 4])
      testpPretty bc parseExpr "1 - 2 + 3 - 4 + 5" $
        C.add
          ( C.sub
              ( C.add
                  ( C.sub
                      (C.const 1 8)
                      (C.const 2 8)
                      8
                  )
                  (C.const 3 8)
                  8
              )
              (C.const 4 8)
              8
          )
          (C.const 5 8)
          8
      testpPretty bc parseExpr "1 * 2 * 3 * 4" $ foldl1 (\x y -> C.mul x y 8) (flip C.const 8 <$> [1, 2, 3, 4])
      failpPretty bc parseExpr "x == y == z"
      failpPretty bc parseExpr "x != y == z"
      testpPretty bc parseExpr "! ! ! 1" $ C.not (C.not (C.not (C.const 1 8) 8) 8) 8
      testpPretty bc parseExpr "1 && 2 && 3 && 4" $ foldr1 (\x y -> C.and x y 8) (flip C.const 8 <$> [1, 2, 3, 4])
      testpPretty bc parseExpr "1 || 2 || 3 || 4" $ foldr1 (\x y -> C.or x y 8) (flip C.const 8 <$> [1, 2, 3, 4])
    it "respects precedence" $ do
      testpPretty bc parseExpr "1 + 2 * 3 - 4" $
        C.sub (C.add (C.const 1 8) (C.mul (C.const 2 8) (C.const 3 8) 8) 8) (C.const 4 8) 8
      testpPretty bc parseExpr "1 == 2 * 3" $
        C.cmpE (C.const 1 8) (C.mul (C.const 2 8) (C.const 3 8) 8) 8
      testpPretty bc parseExpr "2 * 3 == 1" $
        C.cmpE (C.mul (C.const 2 8) (C.const 3 8) 8) (C.const 1 8) 8
      testpPretty bc parseExpr "!!1==2" $
        C.not (C.not (C.cmpE (C.const 1 8) (C.const 2 8) 8) 8) 8
      testpPretty bc parseExpr "!!1==2&&!!1==2" $
        C.and
          (C.not (C.not (C.cmpE (C.const 1 8) (C.const 2 8) 8) 8) 8)
          (C.not (C.not (C.cmpE (C.const 1 8) (C.const 2 8) 8) 8) 8)
          8
      testpPretty bc parseExpr "1&&2||3&&4" $
        C.or (C.and (C.const 1 8) (C.const 2 8) 8) (C.and (C.const 3 8) (C.const 4 8) 8) 8
      testpPretty bc parseExpr "1&&[2||3]&&4" $
        C.and (C.const 1 8) (C.and (C.load (C.or (C.const 2 8) (C.const 3 8) 8) 8) (C.const 4 8) 8) 8

    it "unambiguously parses expressions involving symbols with weird characters" $ do
      let weirdSym1 = "a_1#b'2$c:d.4!e~5"
          weirdVar1 = C.var weirdSym1 8
      testpPretty bc parseExpr ("!" <> weirdSym1 <> "||!a") $ C.or (C.not weirdVar1 8) (C.not (C.var "a" 8) 8) 8
