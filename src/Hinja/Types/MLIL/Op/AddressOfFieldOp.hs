module Hinja.Types.MLIL.Op.AddressOfFieldOp where

import Hinja.Prelude

import Hinja.Types.Variable (Variable)

data AddressOfFieldOp expr = AddressOfFieldOp
    { _addressOfFieldOpSrc :: Variable
    , _addressOfFieldOpOffset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
