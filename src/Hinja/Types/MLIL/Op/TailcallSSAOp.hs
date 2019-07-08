module Hinja.Types.MLIL.Op.TailcallSSAOp where

import Hinja.Prelude


data TailcallSSAOp expr = TailcallSSAOp
    { _tailcallSSAOpOutput :: expr
    , _tailcallSSAOpDest :: expr
    , _tailcallSSAOpParams :: [expr]
    , _tailcallSSAOpSrc_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
