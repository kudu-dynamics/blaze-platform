module Binja.Types.MLIL.Op.CallSSAOp where

import Binja.Prelude


data CallSSAOp expr = CallSSAOp
    { _callSSAOpOutput :: expr
    , _callSSAOpDest :: expr
    , _callSSAOpParams :: [expr]
    , _callSSAOpSrc_memory :: Int64
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
