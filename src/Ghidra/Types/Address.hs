module Ghidra.Types.Address where

import Ghidra.Prelude


type AddressSpaceMap = HashMap AddressSpaceId AddressSpace

newtype AddressSpaceId = AddressSpaceId Int32
  deriving (Generic)
  deriving newtype (Eq, Ord, Read, Show, Num, Hashable)

-- All the address space names in the a1 binary:
-- [".comment",".shstrtab",".strtab",".symtab","EXTERNAL","HASH","OTHER","_elfSectionHeaders","const","ram","register","stack","unique"]

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
  { ptrSize :: Int
  , addressableUnitSize :: Int
  , name :: AddressSpaceName
  } deriving (Eq, Ord, Show, Generic)

data Address = Address
  { space :: AddressSpace
  , offset :: Word64 -- multiply by addressableUnitSize to get byte offset
  } deriving (Eq, Ord, Show, Generic)
