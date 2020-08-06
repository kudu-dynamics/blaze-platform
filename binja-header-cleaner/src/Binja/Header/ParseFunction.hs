{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Binja.Header.ParseFunction where

import           Binja.Header.Prelude                  hiding ( takeWhile )

import Binja.Header.Types.Printer ( Printer, pr )
import           Data.Attoparsec.Text                  ( Parser
                                                       , string
                                                       , takeTill
                                                       )

newtype Function = Function Text
  deriving (Eq, Ord, Read, Show)

parseFunction :: Parser Function
parseFunction = do
  void $ string "BINARYNINJACOREAPI"
  Function <$> takeTill (== '\n')

printFunction :: Function -> Printer ()
printFunction (Function t) = pr t
