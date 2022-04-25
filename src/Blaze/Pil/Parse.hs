{- HLINT ignore "Avoid lambda using `infix`" -}

module Blaze.Pil.Parse (
  Parser,
  ParserCtx (..),
  blankParserCtx,
  mkParserCtx,
  parseVar,
  parseInt,
  parseTerm,
  parseExpr,
  runParser,
  runParser',
  runParserEof,
) where

import Data.Bifunctor (first)
import Prelude (foldr1)

import qualified Blaze.Pil.Construct as C
import Blaze.Prelude hiding (Prefix, first, many, some, takeWhile, try)
import Blaze.Types.Pil (Ctx, Expression, OperationSize, PilVar)

import Blaze.Cfg (getCtxIndices)
import Blaze.Types.Cfg (PilCfg)
import Control.Monad.Combinators.Expr
import qualified Data.Bimap as Bimap
import Data.List.NonEmpty (fromList)
import qualified Data.Set as LazySet
import qualified Data.Text as T
import Text.Megaparsec hiding (runParser, runParser')
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space, string')
import qualified Text.Megaparsec.Char.Lexer as L


newtype ParserCtx = ParserCtx
  { ctxIndices :: Bimap.Bimap Int Ctx
  }
  deriving (Eq, Ord, Show, Generic)

blankParserCtx :: ParserCtx
blankParserCtx =
  ParserCtx
    { ctxIndices = Bimap.empty
    }

mkParserCtx :: PilCfg -> ParserCtx
mkParserCtx cfg =
  ParserCtx
    { ctxIndices = getCtxIndices cfg
    }

type Parser = ParsecT Void Text (Reader ParserCtx)

runParser' :: ParserCtx -> Parser a -> Text -> Either (ParseErrorBundle Text Void) a
runParser' ctx p =
  flip runReader ctx
    . runParserT p ""

runParser :: ParserCtx -> Parser a -> Text -> Either Text a
runParser ctx p =
  first (cs . errorBundlePretty)
    . runParser' ctx p

runParserEof :: ParserCtx -> Parser a -> Text -> Either Text a
runParserEof ctx p = runParser ctx (p <* eof)

lex :: MonadParsec e Text m => m a -> m a
lex = L.lexeme space

binl :: Parser a -> (b -> b -> OperationSize -> b) -> Operator Parser b
binl p f = InfixL (lex p $> (\x y -> f x y 8))

binr :: Parser a -> (b -> b -> OperationSize -> b) -> Operator Parser b
binr p f = InfixR (lex p $> (\x y -> f x y 8))

binn :: Parser a -> (b -> b -> OperationSize -> b) -> Operator Parser b
binn p f = InfixN (lex p $> (\x y -> f x y 8))

-- NOTE if any of these characters get introduced elsewhere in the grammar,
-- they _might_ need to be removed as valid identifier characters. Prefix
-- operators such as "! are one example of characters that can still be
-- valid for identifiers
parseVar :: Parser PilVar
parseVar = lex scan <?> "variable identifier"
  where
    scan :: Parser PilVar
    scan = do
      ctxIndices' <- view #ctxIndices
      firstChar <-
        (letterChar <|> oneOf ['_', '$'])
          <?> "letter or one of _$"
      let restChar =
            (alphaNumChar <|> oneOf ['_', '$', '#', '\'', ':', '.', '!', '~'])
              <?> "alphanumeric character or one of _$#':.!~"
      rest <- T.pack <$> many restChar
      ctxIndex' <- optional (char '@' *> L.decimal)
      let fail =
            failure
              (Just . Label . fromList $ reason)
              (LazySet.singleton . Label . fromList $ if Bimap.null ctxIndices' then expectedEmpty else expected)
            where
              reason = "invalid variable context index: " <> maybe "" show ctxIndex'
              expected = "maximum variable context index: " <> (show . maximum . Bimap.keys $ ctxIndices')
              expectedEmpty = "no contexts in scope"
      case (ctxIndex', Bimap.null ctxIndices') of
        (Nothing, True) -> pure $ C.pilVar (T.cons firstChar rest)
        (Just ctxIndex, False) ->
          case Bimap.lookup ctxIndex ctxIndices' of
            Just ctx -> pure $ C.pilVar' (T.cons firstChar rest) ctx
            Nothing -> fail
        (_, _) -> fail

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
    , (\x -> C.load x 8) <$> between (lex "[") (lex "]") parseExpr
    , (\v -> C.var' v 8) <$> parseVar
    , (\n -> C.const (fromIntegral n) 8) <$> parseInt
    ]

parseExpr :: Parser Expression
parseExpr =
  makeExprParser
    parseTerm
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
