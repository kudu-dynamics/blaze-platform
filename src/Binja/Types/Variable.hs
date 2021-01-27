{-# LANGUAGE TemplateHaskell #-}

module Binja.Types.Variable where

import Binja.Prelude

import Binja.C.Pointers (BNType)
import Binja.C.Enums (BNTypeClass, BNVariableSourceType)


newtype TypeWidth = TypeWidth Bytes
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype TypeAlignment = TypeAlignment Bytes
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

data VarType = VarType
  { _confidence :: Confidence
  , _typeClass :: BNTypeClass
  , _width :: TypeWidth
  , _alignment :: TypeAlignment
  , _signed :: Bool
  , _signedConfidence :: Confidence
  , _isConst :: Bool
  , _constConfidence :: Confidence
  , _typeString :: Text
  , _elementType :: Maybe VarType
  } deriving (Eq, Ord, Show, Generic)

newtype VariableIdentifier = VariableIdentifier Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral, Generic, Hashable)

newtype VariableIndex = VariableIndex Word32
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral, Generic, Hashable)

newtype VariableStorage = VariableStorage Int64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral, Generic, Hashable)

newtype Confidence = Confidence Word8
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral, Generic, Hashable)

data BNVariable = BNVariable
  { _sourceType :: BNVariableSourceType
  , _index :: VariableIndex
  , _storage :: VariableStorage
  } deriving (Eq, Ord, Show)

data Variable = Variable
  { _index :: VariableIndex
  , _name :: Text
  , _storage :: VariableStorage
  , _sourceType :: BNVariableSourceType
  , _varType :: Maybe VarType
  } deriving (Eq, Ord, Show, Generic)

-- TODO: Just using the index for hashing, is this OK? Are variables from multiple 
--       functions with shared index values ever stored in the same hash-using
--       data structure?
instance Hashable Variable where
  hashWithSalt s (Variable index _ _ _ _) = hashWithSalt s index

data BNTypeWithConfidence = BNTypeWithConfidence
  { _bnType :: Maybe BNType
  , _confidence :: Confidence
  } deriving (Eq, Ord, Show)

data BNBoolWithConfidence = BNBoolWithConfidence
  { _value :: Bool
  , _confidence :: Confidence
  } deriving (Eq, Ord, Show)

data BNParameterVariablesWithConfidence = BNParameterVariablesWithConfidence
  { _vars :: [BNVariable]
  , _varCount :: Word64
  , _confidence :: Confidence
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''VarType)
$(makeFieldsNoPrefix ''Variable)
$(makeFieldsNoPrefix ''BNVariable)
$(makeFieldsNoPrefix ''BNTypeWithConfidence)
$(makeFieldsNoPrefix ''BNBoolWithConfidence)
$(makeFieldsNoPrefix ''BNParameterVariablesWithConfidence)
