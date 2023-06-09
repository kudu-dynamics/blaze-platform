module Ghidra.Types.Address where

import Ghidra.Prelude

import Control.Lens (Wrapped)

type AddressSpaceMap = HashMap AddressSpaceId AddressSpace

newtype AddressSpaceId = AddressSpaceId Int32
  deriving (Generic)
  deriving newtype (Eq, Ord, Read, Show, Num, Hashable)

-- TODO: Figure out why this doesn't work when in a deriving clause
instance Wrapped AddressSpaceId

data AddressSpaceName
  = EXTERNAL
  | HASH
  | Const
  | Ram
  | Register
  | Stack
  | Unique
  | Other Text
  deriving (Eq, Ord, Read, Show, Generic, Hashable)

data AddressSpace = AddressSpace
  { id :: AddressSpaceId
  , ptrSize :: Bytes
  , addressableUnitSize :: Bytes
  , name :: AddressSpaceName
  } deriving (Eq, Ord, Show, Generic, Hashable)

data Address = Address
  { space :: AddressSpace
  , offset :: Int64 -- ^ multiply by addressableUnitSize to get byte offset
  } deriving (Eq, Ord, Show, Generic, Hashable)
