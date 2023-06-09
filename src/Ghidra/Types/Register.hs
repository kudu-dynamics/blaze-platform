module Ghidra.Types.Register
  ( module Ghidra.Types.Register
  ) where

import Ghidra.Prelude

data Register = Register
  { name :: Text
  , length :: Bits
  }
  deriving (Show, Generic)
