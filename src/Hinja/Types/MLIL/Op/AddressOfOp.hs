module Hinja.Types.MLIL.Op.AddressOfOp where

import Hinja.Prelude

import Hinja.Types.Variable (Variable)

data AddressOfOp expr = AddressOfOp
    { _addressOfOpSrc :: Variable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
