{-# LANGUAGE DataKinds #-}
module Ghidra.Types.GhidraDataTypes where

import Ghidra.Prelude


data GhidraDataType
    = FloatType     FloatTypeOpts
    | IntType       IntTypeOpts
    | StringType    StringTypeOpts
    | ArrayType     ArrayTypeOpts
    | BadBoyType    BadBoyTypeOpts
    | BoolType      BoolTypeOpts
    | CharType      CharTypeOpts
    | EnumType      EnumTypeOpts
    | PointerType   PointerTypeOpts
    | StructType    StructTypeOpts
    | UnionType     UnionTypeOpts
    | FuncDefType   FuncDefTypeOpts
    | UndefType     UndefTypeOpts
    | OtherType     OtherTypeOpts
    | VoidType      VoidTypeOpts
    | UnknownType
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass Hashable

newtype FloatTypeOpts = FloatTypeOpts
    { width :: Bytes
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

data IntTypeOpts = IntTypeOpts
    { width :: Bytes
    , isSigned :: Bool
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

newtype StringTypeOpts = StringTypeOpts
    { len :: Word32 
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

data ArrayTypeOpts = ArrayTypeOpts
    { elementType :: GhidraDataType
    , elementWidth :: Bytes -- width of data 
    , len :: Word32
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

{- BadBoy corresponds to Ghidra's BadGhidraDataType; it's used for a datatype that is not valid as it
   is used in the program. For example, the class of the underlying datatype may no longer be
   available or may not fit where it has been placed in the program -}
newtype BadBoyTypeOpts = BadBoyTypeOpts
    { width :: Bytes
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

newtype BoolTypeOpts = BoolTypeOpts
    { width :: Bytes
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

newtype CharTypeOpts = CharTypeOpts
    { width :: Bytes
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

data EnumTypeOpts = EnumTypeOpts
    { width :: Bytes
    , enums :: [(Text, Int32)]
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

data PointerTypeOpts = PointerTypeOpts
    { width :: Bytes
    , pointeeType :: GhidraDataType
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

data StructTypeOpts = StructTypeOpts
    { width :: Bytes
    , fields :: [(Int32, GhidraDataType)] -- (offset within struct, datatype)
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

data UnionTypeOpts = UnionTypeOpts
    { width :: Bytes
    , types :: [GhidraDataType]
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

data FuncDefTypeOpts = FuncDefTypeOpts
  { width :: Bytes
  , funcParams :: [FuncParam]
  , callingConvention :: Text
  , returnType :: GhidraDataType -- might not return a value
  } deriving (Eq, Ord, Show, Generic)
    deriving anyclass Hashable

data FuncParam = FuncParam
  { dataType :: GhidraDataType
  , name :: Text
  } deriving (Eq, Ord, Show, Generic)
    deriving anyclass Hashable

{- A datatype that has not been defined yet as a particular type of data in the program -}
newtype UndefTypeOpts = UndefTypeOpts
    { width :: Bytes
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

{- Handles the case for datatypes that we have not implemented (or won't implement, like we probably
   won't implement Ghidra's DiaglogResourceGhidraDataType) -}
data OtherTypeOpts = OtherTypeOpts
    { width :: Bytes
    , name :: Text
    , className :: Text
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

data VoidTypeOpts = VoidTypeOpts
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass Hashable
