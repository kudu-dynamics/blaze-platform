{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Binja.Header.ParseComment where

import           Binja.Prelude                  hiding ( takeWhile )

import           Data.Attoparsec.Text                  ( Parser
                                                       , anyChar
                                                       , manyTill
                                                       , string
                                                       , takeTill
                                                       )
import qualified Data.Text            as Text

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


