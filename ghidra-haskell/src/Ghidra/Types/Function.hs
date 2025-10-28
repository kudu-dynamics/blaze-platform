module Ghidra.Types.Function where

import Ghidra.Prelude
import Ghidra.Types.Address (Address)
import qualified Ghidra.Types as J

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

data Parameter = Parameter
  { handle :: J.Parameter
  , ordinalIndex :: Int32
  , isAutoParameter :: Bool
  , name :: Text
  } deriving (Eq, Ord, Show, Generic)

data HighParameter = HighParameter
  { handle :: J.HighSymbol
  , ordinalIndex :: Int32
  , name :: Text
  } deriving (Eq, Ord, Show, Generic)
