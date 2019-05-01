{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Hinja.Types where

import Hinja.Prelude hiding (onException, handle)

import Hinja.C.Pointers (BNFunction, BNLowLevelILFunction, BNMediumLevelILFunction, BNType)
import Hinja.C.Types (Address)
import Hinja.C.Enums (BNTypeClass, BNVariableSourceType)

data Function = Function
  { _handle :: BNFunction
  , _name :: Text
  , _start :: Address
  } deriving (Eq, Ord, Show)

data LLILFunction = LLILFunction
  { _handle :: BNLowLevelILFunction
  , _func :: Function
  } deriving (Eq, Ord, Show)

data LLILSSAFunction = LLILSSAFunction
  { _handle :: BNLowLevelILFunction
  , _func :: Function
  } deriving (Eq, Ord, Show)

data MLILFunction = MLILFunction
  { _handle :: BNMediumLevelILFunction
  , _func :: Function
  } deriving (Eq, Ord, Show)

data MLILSSAFunction = MLILSSAFunction
  { _handle :: BNMediumLevelILFunction
  , _func :: Function
  } deriving (Eq, Ord, Show)


newtype VarWidth = VarWidth Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype VarAlignment = VarAlignment Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

data VarType = VarType
  { _confidence :: Confidence
  , _typeClass :: BNTypeClass
  , _width :: VarWidth
  , _alignment :: VarAlignment
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
  , _varType :: VarType
  } deriving (Eq, Ord, Show)

data BNTypeWithConfidence = BNTypeWithConfidence
  { _bnType :: Maybe BNType
  , _confidence :: Confidence
  } deriving (Eq, Ord, Show)

data BNBoolWithConfidence = BNBoolWithConfidence
  { _bnBool :: Bool
  , _confidence :: Confidence
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''Function)
$(makeFieldsNoPrefix ''LLILFunction)
$(makeFieldsNoPrefix ''MLILFunction)
$(makeFieldsNoPrefix ''MLILSSAFunction)
$(makeFieldsNoPrefix ''VarType)
$(makeFieldsNoPrefix ''Variable)
$(makeFieldsNoPrefix ''BNVariable)
$(makeFieldsNoPrefix ''BNTypeWithConfidence)
$(makeFieldsNoPrefix ''BNBoolWithConfidence)
