{-# LANGUAGE DataKinds #-}
module Ghidra.Types.DataTypes where

import Ghidra.Prelude hiding (DataType)


data DataType
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
    { elementType :: DataType
    , elementWidth :: Bytes -- width of data 
    , len :: Word32
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

{- BadBoy corresponds to Ghidra's BadDataType; it's used for a datatype that is not valid as it
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

newtype EnumTypeOpts = EnumTypeOpts
    { enums :: [(Text, Int32)]
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

data PointerTypeOpts = PointerTypeOpts
    { width :: Bytes
    , pointeeType :: DataType
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

data StructTypeOpts = StructTypeOpts
    { width :: Bytes
    , fields :: [(Int32, DataType)] -- (offset within struct, datatype)
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

data UnionTypeOpts = UnionTypeOpts
    { width :: Bytes
    , types :: [DataType]
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

data FuncDefTypeOpts = FuncDefTypeOpts
  { width :: Bytes
  , funcParams :: [FuncParam]
  , callingConvention :: Text
  , returnType :: Maybe DataType -- might not return a value
  } deriving (Eq, Ord, Show, Generic)
    deriving anyclass Hashable

data FuncParam = FuncParam
  { dataType :: DataType
  , name :: Text
  } deriving (Eq, Ord, Show, Generic)
    deriving anyclass Hashable

{- A datatype that has not been defined yet as a particular type of data in the program -}
newtype UndefTypeOpts = UndefTypeOpts
    { width :: Bytes
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable

{- Handles the case for datatypes that we have not implemented (or won't implement, like we probably
   won't implement Ghidra's DiaglogResourceDataType) -}
newtype OtherTypeOpts = OtherTypeOpts
    { name :: Text
    } deriving (Eq, Ord, Show, Generic)
      deriving anyclass Hashable


