module Binja.Types.MLIL.Op.TailcallSSAOp where

import Binja.Prelude


data TailcallSSAOp expr = TailcallSSAOp
    { _tailcallSSAOpOutput :: expr
    , _tailcallSSAOpDest :: expr
    , _tailcallSSAOpParams :: [expr]
    , _tailcallSSAOpSrc_memory :: Int64
    }
    deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (Hashable)
