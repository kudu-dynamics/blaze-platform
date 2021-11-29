module Blaze.Pil.ParseSpec where

import Blaze.Pil.Construct as C
import Blaze.Pil.Parse
import Blaze.Prelude
import Blaze.Pretty (PrettyShow (PrettyShow), Tokenizable)
import Data.List (foldl1, foldr1)
import Test.Hspec
import Test.Hspec.Megaparsec (parseSatisfies, shouldFailOn, shouldParse)
import Text.Megaparsec (Parsec, ShowErrorComponent, Stream, TraversableStream, VisualStream, eof, parse)

testp :: (ShowErrorComponent e, Show a, Eq a, Stream s, TraversableStream s, VisualStream s) => Parsec e s a -> s -> a -> Expectation
testp p s expected = parse (p <* eof) "" s `shouldParse` expected

testpPretty :: (ShowErrorComponent e, Tokenizable a, Eq a, Stream s, TraversableStream s, VisualStream s) => Parsec e s a -> s -> a -> Expectation
testpPretty p s expected = parse (PrettyShow <$> p <* eof) "" s `shouldParse` PrettyShow expected

failp :: (ShowErrorComponent e, Stream s, Show a) => Parsec e s a -> s -> Expectation
failp p s = parse (p <* eof) "" `shouldFailOn` s

failpPretty :: (ShowErrorComponent e, Stream s, Tokenizable a) => Parsec e s a -> s -> Expectation
failpPretty p s = parse (PrettyShow <$> p <* eof) "" `shouldFailOn` s

satp :: (ShowErrorComponent e, Eq a, Show a, TraversableStream s, VisualStream s) => Parsec e s a -> s -> (a -> Bool) -> Expectation
satp p s t = parse p "" s `parseSatisfies` t

spec :: Spec
spec = do
  describe "parseVar" $ do
    let varTest name = satp parseVar name (\v -> v ^. #symbol == name)
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
        , "a_1#b'2$c@3:d.4!e~5"
        , "$a0_36#8"
        ]
    it "rejects the empty string" $ do
      failp parseVar ""
    it "rejects invalid identifiers" $ do
      traverse_
        (failp parseVar)
        [ "#"
        , "'"
        , "@"
        , ":"
        , "."
        , "!"
        , "~"
        , "3"
        ]

  describe "parseInt" $ do
    it "does not accept the empty string" $ do
      failp parseInt ""
    context "given binary input" $ do
      it "parses any binary digit" $ do
        testp parseInt "0b01" 1
        testp parseInt "0b10" 2
        testp parseInt "0b10011010100110" 9894
        testp parseInt "0B10011010100110" 9894
      it "parses negative numbers" $ do
        testp parseInt "-0b01" (-1)
        testp parseInt "-0b10" (-2)
        testp parseInt "-0b10011010100110" (-9894)
        testp parseInt "-0B10011010100110" (-9894)
      it "does not allow non-binary characters" $ do
        failp parseInt "0b2"
        failp parseInt "-0b2"
        failp parseInt "0bg"
        failp parseInt "0bG"
        failp parseInt "0b_"
    context "given octal input" $ do
      it "parses any octal digit" $ do
        testp parseInt "0o01234567" 0o01234567
        testp parseInt "0o76543210" 0o76543210
        testp parseInt "0o27645126371265" 0o27645126371265
        testp parseInt "0O27645126371265" 0o27645126371265
      it "parses negative numbers" $ do
        testp parseInt "-0o01234567" (-0o01234567)
        testp parseInt "-0o76543210" (-0o76543210)
        testp parseInt "-0o27645126371265" (-0o27645126371265)
        testp parseInt "-0O27645126371265" (-0o27645126371265)
      it "does not allow non-octal characters" $ do
        failp parseInt "0o8"
        failp parseInt "-0o8"
        failp parseInt "0og"
        failp parseInt "0oG"
        failp parseInt "0o_"
    context "given decimal input" $ do
      it "parses any decimal digit" $ do
        testp parseInt "0n1234567890" 1234567890
        testp parseInt "0N1234567890" 1234567890
        testp parseInt "1234567890" 1234567890
        testp parseInt "340987561033841023486" 340987561033841023486
        testp parseInt "0n340987561033841023486" 340987561033841023486
      it "parses negative numbers" $ do
        testp parseInt "-0n1234567890" (-1234567890)
        testp parseInt "-0N1234567890" (-1234567890)
        testp parseInt "-1234567890" (-1234567890)
        testp parseInt "-340987561033841023486" (-340987561033841023486)
        testp parseInt "-0n340987561033841023486" (-340987561033841023486)
      it "does not allow non-decimal characters" $ do
        failp parseInt "0na"
        failp parseInt "a"
        failp parseInt "-0NA"
        failp parseInt "-0nA"
        failp parseInt "-0na"
        failp parseInt "_"
    context "given hexadecimal input" $ do
      it "parses any hexadecimal digit" $ do
        testp parseInt "0x1234567890abcdefABCDEF" 0x1234567890abcdefABCDEF
        testp parseInt "0X1234567890abcdefABCDEF" 0x1234567890abcdefABCDEF
        testp parseInt "0x81913fabd91Dc02567E" 0x81913fabd91Dc02567E
      it "parses negative numbers" $ do
        testp parseInt "-0x1234567890abcdefABCDEF" (-0x1234567890abcdefABCDEF)
        testp parseInt "-0X1234567890abcdefABCDEF" (-0x1234567890abcdefABCDEF)
        testp parseInt "-0x81913fabd91Dc02567E" (-0x81913fabd91Dc02567E)
      it "does not allow non-hexadecimal characters" $ do
        failp parseInt "-0xg"
        failp parseInt "0xg"
        failp parseInt "-0xG"
        failp parseInt "-0x_"

  describe "parseExpr" $ do
    it "respects associativity" $ do
      testpPretty parseExpr "1 + 2 + 3 + 4" $ foldl1 (\x y -> C.add x y 8) (flip C.const 8 <$> [1, 2, 3, 4])
      testpPretty parseExpr "1 - 2 - 3 - 4" $ foldl1 (\x y -> C.sub x y 8) (flip C.const 8 <$> [1, 2, 3, 4])
      testpPretty parseExpr "1 - 2 + 3 - 4 + 5" $
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
      testpPretty parseExpr "1 * 2 * 3 * 4" $ foldl1 (\x y -> C.mul x y 8) (flip C.const 8 <$> [1, 2, 3, 4])
      failpPretty parseExpr "x == y == z"
      failpPretty parseExpr "x != y == z"
      testpPretty parseExpr "! ! ! 1" $ C.not (C.not (C.not (C.const 1 8) 8) 8) 8
      testpPretty parseExpr "1 && 2 && 3 && 4" $ foldr1 (\x y -> C.and x y 8) (flip C.const 8 <$> [1, 2, 3, 4])
      testpPretty parseExpr "1 || 2 || 3 || 4" $ foldr1 (\x y -> C.or x y 8) (flip C.const 8 <$> [1, 2, 3, 4])
    it "respects precedence" $ do
      testpPretty parseExpr "1 + 2 * 3 - 4" $
        C.sub (C.add (C.const 1 8) (C.mul (C.const 2 8) (C.const 3 8) 8) 8) (C.const 4 8) 8
      testpPretty parseExpr "1 == 2 * 3" $
        C.cmpE (C.const 1 8) (C.mul (C.const 2 8) (C.const 3 8) 8) 8
      testpPretty parseExpr "2 * 3 == 1" $
        C.cmpE (C.mul (C.const 2 8) (C.const 3 8) 8) (C.const 1 8) 8
      testpPretty parseExpr "!!1==2" $
        C.not (C.not (C.cmpE (C.const 1 8) (C.const 2 8) 8) 8) 8
      testpPretty parseExpr "!!1==2&&!!1==2" $
        C.and
          (C.not (C.not (C.cmpE (C.const 1 8) (C.const 2 8) 8) 8) 8)
          (C.not (C.not (C.cmpE (C.const 1 8) (C.const 2 8) 8) 8) 8)
          8
      testpPretty parseExpr "1&&2||3&&4" $
        C.or (C.and (C.const 1 8) (C.const 2 8) 8) (C.and (C.const 3 8) (C.const 4 8) 8) 8
      testpPretty parseExpr "1&&[2||3]&&4" $
        C.and (C.const 1 8) (C.and (C.load (C.or (C.const 2 8) (C.const 3 8) 8) 8) (C.const 4 8) 8) 8

    it "unambiguously parses expressions involving symbols with weird characters" $ do
      let weirdSym1 = "a_1#b'2$c@3:d.4!e~5"
          weirdVar1 = C.var weirdSym1 8
      testpPretty parseExpr ("!" <> weirdSym1 <> "||!a") $ C.or (C.not weirdVar1 8) (C.not (C.var "a" 8) 8) 8
