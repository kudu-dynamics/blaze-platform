{-# LANGUAGE TemplateHaskell #-}

module Binja.Types.TypeLibrary where

import Binja.C.Enums (BNVariableSourceType)
import Binja.C.Pointers (BNType)
import Binja.Prelude
import Binja.Types.Variable (BNVariable, Confidence, VariableIndex, VariableStorage, VarType)

data BNQualifiedNameAndType
  = BNQualifiedNameAndType
      { _name :: [Text],
        _join :: Text,
        _nameCount :: Word64,
        _qnType :: Maybe BNType
      }
  deriving (Eq, Ord, Show)

data BNFunctionParameter
  = BNFunctionParameter
      { _name :: Text,
        _fpType :: Maybe BNType,
        _typeConfidence :: Confidence,
        _defaultLocation :: Maybe Bool,
        _sourceType :: Maybe BNVariableSourceType,
        _index :: Maybe VariableIndex,
        _storage :: Maybe VariableStorage
      }
  deriving (Eq, Ord, Show)

data FunctionT
  = FunctionT
      { _name :: Text,
        _returnType :: Maybe VarType,
        _argumentTypes :: [Maybe VarType]
      }
  deriving (Eq, Ord, Show)

-- $(makeFieldsNoPrefix ''BNQualifiedNameAndType)
$(makeFieldsNoPrefix ''BNFunctionParameter)
$(makeFieldsNoPrefix ''FunctionT)
