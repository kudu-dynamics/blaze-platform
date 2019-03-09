{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Hinja.ParseStruct where

-- import qualified Prelude              as P
import           Hinja.Prelude                  hiding ( takeWhile )

import Hinja.Types.Printer ( Printer, pr, indent )
import           Data.Attoparsec.Text                  ( Parser
                                                       , anyChar
                                                       , char
                                                       , decimal
                                                       , endOfInput
                                                       , letter
                                                       , manyTill
                                                       , many1
                                                       , parseOnly
                                                       , satisfy
                                                       , sepBy1
                                                       , skipSpace
                                                       , space
                                                       , string
                                                       , takeTill
                                                       , takeWhile
                                                       )
import           Data.Char                             ( isAlpha
                                                       , isAlphaNum
                                                       , isSpace
                                                       , isUpper
                                                       )
import qualified Data.Char            as Char
import qualified Data.Text            as Text
import qualified Data.Text.IO         as TextIO

type StructField = Text

-- really need to grab the field names out...

-- data StructField = StructField
--   { fieldName :: Text
--   , fieldType :: Text
--   }

data Struct = Struct
  { name :: Text
  , fields :: Maybe [StructField]
  } deriving (Eq, Ord, Read, Show)


printStructField :: StructField -> Printer ()
printStructField sf = pr sf

printStruct :: Struct -> Printer ()
printStruct s = case fields s of
  Nothing -> pr $ "typedef struct " <> name s <> " " <> name s <> ";"
  Just fs -> do
    pr $ "typedef struct " <> name s
    pr "{"
    indent $ mapM_ printStructField fs
    pr $ "} " <> name s <> ";"
       

validCName :: Parser Text
validCName = do
  start <- satisfy isAlpha
  rest <- takeWhile (\c -> isAlphaNum c || c == '_')
  return $ Text.cons start rest

parseStruct :: Parser Struct
parseStruct = do
  string "struct" >> skipSpace
  n <- validCName
  skipSpace
  mf <- (Just <$> parseFields) <|> return Nothing
  void $ char ';'
  return $ Struct { name = n
                  , fields = mf }


parseFields :: Parser [StructField]
parseFields = do
  void $ char '{'
  fs <- many1 parseField
  skipSpace
  void $ char '}'
  return fs
  
parseField :: Parser StructField
parseField = do
  skipSpace
  takeTill (== '\n') <* char '\n'
  
--  takeTill \c -> c == ';' || c == '}'

demo0 :: Text
demo0 = "struct BNTransformParameter;"

demo1 :: Text
demo1 = "struct BNTransformParameter\n {\n const char* name;\n BNDataBuffer* value;\n };"
