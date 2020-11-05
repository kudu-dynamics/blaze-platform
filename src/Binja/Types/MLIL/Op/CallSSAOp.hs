module Binja.Types.MLIL.Op.CallSSAOp where

import Binja.Prelude
import Test.SmallCheck.Series (Serial)


data CallSSAOp expr = CallSSAOp
    { _callSSAOpOutput :: expr
    , _callSSAOpDest :: expr
    , _callSSAOpParams :: [expr]
    , _callSSAOpSrc_memory :: Int64
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Serial m a => Serial m (CallSSAOp a)
instance Hashable a => Hashable (CallSSAOp a)