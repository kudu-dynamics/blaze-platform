module Hinja.Types.MLIL.Op.CallSSAOp where

import Hinja.Prelude


data CallSSAOp expr = CallSSAOp
    { _callSSAOpOutput :: expr
    , _callSSAOpDest :: expr
    , _callSSAOpParams :: [expr]
    , _callSSAOpSrc_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
