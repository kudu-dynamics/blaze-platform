module Binja.Types.MLIL.Op.TailcallSSAOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data TailcallSSAOp expr = TailcallSSAOp
    { _tailcallSSAOpOutput :: expr
    , _tailcallSSAOpDest :: expr
    , _tailcallSSAOpParams :: [expr]
    , _tailcallSSAOpSrc_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (TailcallSSAOp a)
instance Hashable a => Hashable (TailcallSSAOp a)