{-# LANGUAGE DataKinds #-}
module Ghidra.Types.Variable where

import Ghidra.Prelude hiding (DataType)

import Ghidra.Types.Address (Address)


data VarType
  = Const Int64
  | Addr
    { location :: Address
    , pcAddress :: Maybe Address
    }
  deriving (Eq, Ord, Show, Generic, Hashable)

data VarNode = VarNode
  { varType :: VarType
  , size :: Bytes
  } deriving (Eq, Ord, Show, Generic, Hashable)

type Slot = Word64

data HighVariableType
  = HighConstant Int64
  | HighGlobal
  | HighLocal
  | HighParam Slot
  | HighOther
  deriving (Eq, Ord, Show, Generic, Hashable)

newtype DataType = DataType { name :: Text }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

data HighVariable = HighVariable
  { dataType :: DataType
  , name :: Maybe Text
  , size :: Bytes
  , highVariableType :: HighVariableType
  } deriving (Eq, Ord, Show, Generic, Hashable)

data HighVarNode = HighVarNode
  { varType :: VarType
  , size :: Bytes

  -- Address where varnode is defined,
  -- or Nothing if varnode is an input to func call
  , pcAddress :: Maybe Address
  , highVariable :: Maybe HighVariable
  } deriving (Eq, Ord, Show, Generic, Hashable)
