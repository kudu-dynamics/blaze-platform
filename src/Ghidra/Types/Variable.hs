{-# LANGUAGE DataKinds #-}
module Ghidra.Types.Variable where

import Ghidra.Prelude hiding (DataType)

import Ghidra.Types.Address (Address)


data VarType
  = Const Int64
  | Addr Address
  deriving (Eq, Ord, Show, Generic, Hashable)

data VarNode = VarNode
  { varType :: VarType
  , size :: Bytes
  -- , highVariable :: Maybe HighVariable
  } deriving (Eq, Ord, Show, Generic)

type Slot = Word64

data HighVariableType
  = HighConstant Int64
  | HighGlobal
  | HighLocal
  | HighParam Slot
  | HighOther
  deriving (Eq, Ord, Show, Generic)

data DataType = DataType { name :: Text }
  deriving (Eq, Ord, Show, Generic)

data HighVariable = HighVariable
  { dataType :: DataType
  , name :: Maybe Text
  , size :: Bytes
  , highVariableType :: HighVariableType
  } deriving (Eq, Ord, Show, Generic)

data HighVarNode = HighVarNode
  { varType :: VarType
  , size :: Bytes
  , highVariable :: Maybe HighVariable
  } deriving (Eq, Ord, Show, Generic)
