{-# LANGUAGE TemplateHaskell #-}

module Binja.Types.Function where

import Binja.Prelude hiding (handle)

import Binja.C.Pointers (BNFunction, BNLowLevelILFunction, BNMediumLevelILFunction)

-- TODO: Add a symbol field
data Function = Function
  { _handle :: BNFunction
  , _name :: Text
  , _start :: Address
  } deriving (Ord, Show, Generic)

instance Eq Function where
  (==) a b = _start a == _start b

instance Hashable Function where
  hashWithSalt s x = hashWithSalt s $ _start x

data LLILFunction = LLILFunction
  { _handle :: BNLowLevelILFunction
  , _func :: Function
  } deriving (Ord, Show)

instance Eq LLILFunction where
  (==) (LLILFunction _ funcA) (LLILFunction _ funcB) = funcA == funcB

instance Hashable LLILFunction where
  hashWithSalt s (LLILFunction _ func) = hashWithSalt s func

data LLILSSAFunction = LLILSSAFunction
  { _handle :: BNLowLevelILFunction
  , _func :: Function
  } deriving (Ord, Show)

instance Eq LLILSSAFunction where
  (==) (LLILSSAFunction _ funcA) (LLILSSAFunction _ funcB) = funcA == funcB

instance Hashable LLILSSAFunction where
  hashWithSalt s (LLILSSAFunction _ func) = hashWithSalt s func

data MLILFunction = MLILFunction
  { _handle :: BNMediumLevelILFunction
  , _func :: Function
  } deriving (Ord, Show)

instance Eq MLILFunction where
  (==) (MLILFunction _ funcA) (MLILFunction _ funcB) = funcA == funcB

instance Hashable MLILFunction where
  hashWithSalt s (MLILFunction _ func) = hashWithSalt s func

data MLILSSAFunction = MLILSSAFunction
  { _handle :: BNMediumLevelILFunction
  , _func :: Function
  } deriving (Ord, Show)

instance Eq MLILSSAFunction where
  (==) (MLILSSAFunction _ funcA) (MLILSSAFunction _ funcB) = funcA == funcB

instance Hashable MLILSSAFunction where
  hashWithSalt s (MLILSSAFunction _ func) = hashWithSalt s func


$(makeFieldsNoPrefix ''Function)
$(makeFieldsNoPrefix ''LLILFunction)
$(makeFieldsNoPrefix ''LLILSSAFunction)
$(makeFieldsNoPrefix ''MLILFunction)
$(makeFieldsNoPrefix ''MLILSSAFunction)
