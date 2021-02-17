module Binja.Types.MLIL.Op.GotoOp where

import Binja.Prelude


newtype GotoOp expr = GotoOp
    { _gotoOpDest :: Int64
    }
    deriving stock (Show, Generic, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Hashable)
