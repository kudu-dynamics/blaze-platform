{- HLINT ignore "Use newtype instead of data" -}
module Blaze.Types.Pil.Op.GlobalPtrOp where

import Blaze.Prelude

data GlobalPtrOp expr = GlobalPtrOp
  { constant :: Int64
  , symbol :: Maybe Text
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON, Serialize)

instance Hashable a => Hashable (GlobalPtrOp a)
