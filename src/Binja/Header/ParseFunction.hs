{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Binja.Header.ParseFunction where

import           Binja.Prelude                  hiding ( takeWhile )

import Binja.Types.Printer ( Printer, pr )
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
