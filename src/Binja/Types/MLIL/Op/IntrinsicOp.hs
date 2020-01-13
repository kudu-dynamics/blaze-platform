module Binja.Types.MLIL.Op.IntrinsicOp where

import Binja.Prelude

import Binja.Types.MLIL.Common (Intrinsic)
import Binja.Types.Variable (Variable)

data IntrinsicOp expr = IntrinsicOp
    { _intrinsicOpOutput :: [Variable]
    , _intrinsicOpIntrinsic :: Intrinsic
    , _intrinsicOpParams :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (IntrinsicOp a)