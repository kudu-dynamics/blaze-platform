{-# LANGUAGE TemplateHaskell #-}

module Binja.Types.StringReference where

import Binja.Prelude

import Binja.C.Enums (BNStringType)
import Binja.C.Types (Address)

data BNStringReference
  = BNStringReference
      { _stringType :: BNStringType,
        _start :: Address,
        _length :: Word64
      }
  deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''BNStringReference)