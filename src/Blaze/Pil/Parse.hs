{- HLINT ignore "Avoid lambda using `infix`" -}

module Blaze.Pil.Parse (
  Parser,
  parseVar,
  parseInt,
  parseTerm,
  parseExpr,
  run,
) where

import Prelude (foldr1)
import Data.Bifunctor (first)

import qualified Blaze.Pil.Construct as C
import Blaze.Prelude hiding (Prefix, first, many, some, takeWhile, try)
import Blaze.Types.Pil (Expression, OperationSize, PilVar)

import Control.Monad.Combinators.Expr
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

lex :: MonadParsec e Text m => m a -> m a
lex = L.lexeme C.space

binl :: Parser a -> (b -> b -> OperationSize -> b) -> Operator Parser b
binl p f = InfixL (lex p $> (\x y -> f x y 8))

binr :: Parser a -> (b -> b -> OperationSize -> b) -> Operator Parser b
binr p f = InfixR (lex p $> (\x y -> f x y 8))

binn :: Parser a -> (b -> b -> OperationSize -> b) -> Operator Parser b
binn p f = InfixN (lex p $> (\x y -> f x y 8))

parseVar :: Parser PilVar
parseVar = lex scan <?> "variable identifier"
  where
    firstChar = satisfy (\c -> isLetter c || c == '_') <?> "alphanumeric character or underscore"
    restChar = (alphaNumChar <|> oneOf ['_', '#', '\'']) <?> "alphanumeric character or one of _#'"
    rest = T.pack <$> many restChar
    scan = (\x y -> C.pilVar $ T.cons x y) <$> firstChar <*> rest

parseInt :: Parser Integer
parseInt =
  lex . L.signed (void "") $
    choice
      [ string' "0x" *> L.hexadecimal
      , string' "0o" *> L.octal
      , string' "0b" *> L.binary
      , optional (string' "0n") *> L.decimal
      ]

parseTerm :: Parser Expression
parseTerm =
  choice
    [ between (lex "(") (lex ")") parseExpr
    , (\v -> C.var' v 8) <$> parseVar
    , (\n -> C.const (fromIntegral n) 8) <$> parseInt
    ]

parseExpr :: Parser Expression
parseExpr =
  makeExprParser parseTerm
    [
      [ binl "*" C.mul
      ]
    ,
      [ binl "+" C.add
      , binl "-" C.sub
      ]
    ,
      [ binn "==" C.cmpE
      , binn "!=" C.cmpNE
      , binn (optional "u" *> ">=") C.cmpUge
      , binn (optional "u" *> ">") C.cmpUgt
      , binn (optional "u" *> "<=") C.cmpUle
      , binn (optional "u" *> "<") C.cmpUlt
      , binn "s>=" C.cmpSge
      , binn "s>" C.cmpSgt
      , binn "s<=" C.cmpSle
      , binn "s<" C.cmpSlt
      ]
    ,
      [ Prefix (foldr1 (.) <$> some (lex "!" $> (\x -> C.not x 8)))
      ]
    ,
      [ binr "&&" C.and
      ]
    ,
      [ binr "||" C.or
      ]
    ]

run :: Parser a -> Text -> Either Text a
run p = first (cs . errorBundlePretty) . parse (p <* eof) ""
