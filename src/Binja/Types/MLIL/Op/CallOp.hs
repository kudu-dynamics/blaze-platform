module Binja.Types.MLIL.Op.CallOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data CallOp expr = CallOp
    { _callOpOutput :: [Variable]
    , _callOpDest :: expr
    , _callOpParams :: [expr]
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (CallOp a)
instance Serial m a => Serial m (CallOp a)
