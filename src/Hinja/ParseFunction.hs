{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Hinja.ParseFunction where

import qualified Prelude              as P
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

-- data Function = Function
--   { returnType :: Text
--   , name :: Text
--   } deriving (Eq, Ord, Read, Show)

newtype Function = Function Text
  deriving (Eq, Ord, Read, Show)

parseFunction :: Parser Function
parseFunction = do
  void $ string "BINARYNINJACOREAPI"
  Function <$> takeTill (== '\n')

printFunction :: Function -> Printer ()
printFunction (Function t) = pr t
