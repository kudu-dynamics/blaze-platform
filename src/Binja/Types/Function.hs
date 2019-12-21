{-# LANGUAGE TemplateHaskell #-}

module Binja.Types.Function where

import Binja.Prelude hiding (handle)

import Binja.C.Pointers (BNFunction, BNLowLevelILFunction, BNMediumLevelILFunction)
import Binja.C.Types (Address)

data Function = Function
  { _handle :: BNFunction
  , _name :: Text
  , _start :: Address
  } deriving (Eq, Ord, Show, Generic)

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

$(makeFieldsNoPrefix ''Function)
$(makeFieldsNoPrefix ''LLILFunction)
$(makeFieldsNoPrefix ''MLILFunction)
$(makeFieldsNoPrefix ''MLILSSAFunction)
