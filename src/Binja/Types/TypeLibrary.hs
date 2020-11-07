{-# LANGUAGE TemplateHaskell #-}

module Binja.Types.TypeLibrary where

import Binja.C.Pointers (BNType)
import Binja.Prelude
import Binja.Types.Variable
  ( BNVariable
  , Confidence
  , VarType
  )

data BNQualifiedNameAndType = BNQualifiedNameAndType
  { _name :: [Text]
  , _join :: Text
  , _nameCount :: Word64
  , _bnTypePtr :: Maybe BNType
  }
  deriving (Eq, Ord, Show)

data BNFunctionParameter = BNFunctionParameter
  { _name :: Text
  , _bnTypePtr :: Maybe BNType
  , _typeConfidence :: Confidence
  , _defaultLocation :: Bool
  , _location :: BNVariable
  }
  deriving (Eq, Ord, Show)

data FunctionType = FunctionType
  { _name :: Text
  , _qualifiedName :: [Text] -- all examples had just 1 name, but just case...
  , _returnType :: VarType
  , _argumentTypes :: [VarType]
  }
  deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''BNQualifiedNameAndType)
$(makeFieldsNoPrefix ''BNFunctionParameter)
$(makeFieldsNoPrefix ''FunctionType)
