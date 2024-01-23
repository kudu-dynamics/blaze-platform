module Binja.Types.MLIL.Op.SetVarOp where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data SetVarOp expr = SetVarOp
    { _setVarOpDest :: Variable
    , _setVarOpSrc :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (SetVarOp a)
