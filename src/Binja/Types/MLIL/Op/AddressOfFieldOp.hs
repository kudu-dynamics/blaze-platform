module Binja.Types.MLIL.Op.AddressOfFieldOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data AddressOfFieldOp expr = AddressOfFieldOp
    { _addressOfFieldOpSrc :: Variable
    , _addressOfFieldOpOffset :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (AddressOfFieldOp a)
instance Serial m a => Serial m (AddressOfFieldOp a)