module Binja.Types.MLIL.Op.AddressOfFieldOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data AddressOfFieldOp expr = AddressOfFieldOp
    { _addressOfFieldOpSrc :: Variable
    , _addressOfFieldOpOffset :: Int64
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
