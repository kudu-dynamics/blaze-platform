module Ghidra.Types.BasicBlock where

import Ghidra.Prelude
import Ghidra.Types.Address (Address)
import qualified Ghidra.Types as J

data BasicBlock = BasicBlock
  { handle :: J.CodeBlock
  , startAddress :: Address
  } deriving (Show, Generic)

instance Eq BasicBlock where
  a == b = a ^. #startAddress == b ^. #startAddress

instance Ord BasicBlock where
  compare a b = compare (a ^. #startAddress) (b ^. #startAddress)

data BasicBlockGraph a = BasicBlockGraph
  { nodes :: [a]
  , edges :: [(a, a)]
  } deriving (Eq, Ord, Show, Hashable, Generic, Functor, Foldable, Traversable)

