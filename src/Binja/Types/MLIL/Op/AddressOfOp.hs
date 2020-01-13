module Binja.Types.MLIL.Op.AddressOfOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data AddressOfOp expr = AddressOfOp
    { _addressOfOpSrc :: Variable
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (AddressOfOp a)