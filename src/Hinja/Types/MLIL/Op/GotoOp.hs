module Hinja.Types.MLIL.Op.GotoOp where

import Hinja.Prelude


data GotoOp expr = GotoOp
    { _gotoOpDest :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
