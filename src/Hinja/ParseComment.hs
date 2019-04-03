{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Hinja.ParseComment where

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

data Comment = Single Text
             | Multi Text
             deriving (Eq, Ord, Read, Show)

parseSingleComment :: Parser Text
parseSingleComment = do
  void $ string "//"
  takeTill (== '\n')

parseMultiComment :: Parser Text
parseMultiComment = do
  void $ string "/*"
  Text.pack <$> manyTill anyChar (string "*/")

parseComment :: Parser Comment
parseComment = (Single <$> parseSingleComment)
  <|> (Multi <$> parseMultiComment)


