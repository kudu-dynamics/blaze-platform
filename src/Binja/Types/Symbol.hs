{-# LANGUAGE TemplateHaskell #-}

module Binja.Types.Symbol where

import Binja.Prelude

data BNNameSpace
  = BNNameSpace
      { _name :: [Text],
        _join :: Text,
        _nameCount :: Word64
      }
  deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''BNNameSpace)
