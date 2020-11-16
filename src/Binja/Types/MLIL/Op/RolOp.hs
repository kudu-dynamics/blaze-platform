module Binja.Types.MLIL.Op.RolOp where

import Binja.Prelude


data RolOp expr = RolOp
    { _rolOpLeft :: expr
    , _rolOpRight :: expr
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (RolOp a)
instance Serial m a => Serial m (RolOp a)