module Hinja.Types.Function where

import Hinja.Prelude hiding (handle)

import Hinja.C.Pointers (BNFunction, BNLowLevelILFunction, BNMediumLevelILFunction)
import Hinja.C.Types (Address)

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

$(makeFieldsNoPrefix ''Function)
$(makeFieldsNoPrefix ''LLILFunction)
$(makeFieldsNoPrefix ''MLILFunction)
$(makeFieldsNoPrefix ''MLILSSAFunction)
