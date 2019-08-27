{-# LANGUAGE TemplateHaskell #-}

module Binja.Types.Variable where

import Binja.Prelude

import Binja.C.Pointers (BNType)
import Binja.C.Enums (BNTypeClass, BNVariableSourceType)

newtype TypeWidth = TypeWidth Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype TypeAlignment = TypeAlignment Word64
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
  } deriving (Eq, Ord, Show)

newtype VariableIdentifier = VariableIdentifier Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype VariableIndex = VariableIndex Word32
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype VariableStorage = VariableStorage Int64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype Confidence = Confidence Word8
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

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
  } deriving (Eq, Ord, Show)

data BNTypeWithConfidence = BNTypeWithConfidence
  { _bnType :: Maybe BNType
  , _confidence :: Confidence
  } deriving (Eq, Ord, Show)

data BNBoolWithConfidence = BNBoolWithConfidence
  { _value :: Bool
  , _confidence :: Confidence
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''VarType)
$(makeFieldsNoPrefix ''Variable)
$(makeFieldsNoPrefix ''BNVariable)
$(makeFieldsNoPrefix ''BNTypeWithConfidence)
$(makeFieldsNoPrefix ''BNBoolWithConfidence)
