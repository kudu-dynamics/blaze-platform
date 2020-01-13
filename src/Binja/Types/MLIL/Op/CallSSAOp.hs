module Binja.Types.MLIL.Op.CallSSAOp where

import Binja.Prelude


data CallSSAOp expr = CallSSAOp
    { _callSSAOpOutput :: expr
    , _callSSAOpDest :: expr
    , _callSSAOpParams :: [expr]
    , _callSSAOpSrc_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (CallSSAOp a)