module Binja.Types.MLIL.Op.TailcallSSAOp where

import Binja.Prelude


data TailcallSSAOp expr = TailcallSSAOp
    { _tailcallSSAOpOutput :: expr
    , _tailcallSSAOpDest :: expr
    , _tailcallSSAOpParams :: [expr]
    , _tailcallSSAOpSrc_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (TailcallSSAOp a)
