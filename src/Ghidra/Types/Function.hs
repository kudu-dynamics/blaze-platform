module Ghidra.Types.Function where

import Ghidra.Prelude
import qualified Ghidra.Types as J
import Ghidra.Types.Address (Address)

data Function = Function
  { handle :: J.Function
  , startAddress :: Address
  } deriving (Show, Generic)

instance Eq Function where
  a == b = a ^. #startAddress == b ^. #startAddress

instance Ord Function where
  compare a b = compare (a ^. #startAddress) (b ^. #startAddress)

newtype Thunk = Thunk J.Function
  deriving (Eq, Ord, Show, Generic)
