module Ghidra.Types.Address where

import Ghidra.Prelude


type AddressSpaceMap = HashMap AddressSpaceId AddressSpace

newtype AddressSpaceId = AddressSpaceId Int32
  deriving (Generic)
  deriving newtype (Eq, Ord, Read, Show, Num, Hashable)

data AddressSpace = AddressSpace
  { ptrSize :: Int
  , addressableUnitSize :: Int
  , name :: Text
  } deriving (Eq, Ord, Show, Generic)

data Address = Address
  { space :: AddressSpace
  , offset :: Word64 -- multiply by addressableUnitSize to get byte offset
  } deriving (Eq, Ord, Show, Generic)
