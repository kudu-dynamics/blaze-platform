module Binja.Types.MLIL.Op.AddressOfOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

newtype AddressOfOp expr = AddressOfOp
    { _addressOfOpSrc :: Variable
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
