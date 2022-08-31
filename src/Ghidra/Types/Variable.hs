{-# LANGUAGE DataKinds #-}
module Ghidra.Types.Variable where

import Ghidra.Prelude hiding (DataType)

import Ghidra.Types.Address (Address)


data VarType
  = Const Int64
  | Addr Address
  deriving (Eq, Ord, Show, Generic)

data VarNode = VarNode
  { varType :: VarType
  , size :: Bytes
  } deriving (Eq, Ord, Show, Generic)

data HighVariableType
  = HighConstant Int64
  | HighGlobal
  | HighLocal
  | HighOther
  deriving (Eq, Ord, Show, Generic)

data DataType = DataType { name :: Text }
  deriving (Eq, Ord, Show, Generic)

data HighVariable = HighVariable
  { dataType :: DataType
  , symbol :: Text
  , size :: Bytes
  , highVariableType :: HighVariableType
  } deriving (Eq, Ord, Show, Generic)

data HighVarNode = HighVarNode
  { varType :: VarType
  , size :: Bytes
  , highVariable :: HighVariable
  } deriving (Eq, Ord, Show, Generic)
