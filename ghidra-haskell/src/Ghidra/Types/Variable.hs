{-# LANGUAGE DataKinds #-}
module Ghidra.Types.Variable where

import Ghidra.Prelude
import Ghidra.Types.Address (Address)
import Ghidra.Types.GhidraDataTypes (GhidraDataType)

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

data HighSymbol = HighSymbol
  { name :: Maybe Text
  , isParameter :: Bool
  } deriving (Eq, Ord, Show, Generic, Hashable)

data HighVariable = HighVariable
  { dataType :: GhidraDataType
  , name :: Maybe Text
  , size :: Bytes
  , highVariableType :: HighVariableType
  , highSymbol :: Maybe HighSymbol
  } deriving (Eq, Ord, Show, Generic, Hashable)

data HighVarNode = HighVarNode
  { varType :: VarType
  , size :: Bytes

  -- Address where varnode is defined,
  -- or Nothing if varnode is an input to func call
  , pcAddress :: Maybe Address
  , highVariable :: Maybe HighVariable
  } deriving (Eq, Ord, Show, Generic, Hashable)
