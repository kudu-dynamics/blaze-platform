{-# LANGUAGE TemplateHaskell #-}

module Binja.Types.StringReference where

import Binja.Prelude

import Binja.C.Enums (BNStringType)

data BNStringReference
  = BNStringReference
      { _stringType :: BNStringType,
        _start :: Address,
        _length :: Bytes
      }
  deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''BNStringReference)
