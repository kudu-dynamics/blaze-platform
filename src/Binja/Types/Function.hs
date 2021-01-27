{-# LANGUAGE TemplateHaskell #-}

module Binja.Types.Function where

import Binja.Prelude hiding (handle)

import Binja.C.Pointers (BNFunction, BNLowLevelILFunction, BNMediumLevelILFunction)

import qualified Data.Text as Text


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
  (==) a b = _func (a :: LLILFunction) == _func (b :: LLILFunction)

instance Hashable LLILFunction where
  hashWithSalt s x = hashWithSalt s $ _func (x :: LLILFunction)

data LLILSSAFunction = LLILSSAFunction
  { _handle :: BNLowLevelILFunction
  , _func :: Function
  } deriving (Ord, Show)

instance Eq LLILSSAFunction where
  (==) a b = _func (a :: LLILSSAFunction) == _func (b :: LLILSSAFunction)

instance Hashable LLILSSAFunction where
  hashWithSalt s x = hashWithSalt s $ _func (x :: LLILSSAFunction)

data MLILFunction = MLILFunction
  { _handle :: BNMediumLevelILFunction
  , _func :: Function
  } deriving (Ord, Show)

instance Eq MLILFunction where
  (==) a b = _func (a :: MLILFunction) == _func (b :: MLILFunction)

instance Hashable MLILFunction where
  hashWithSalt s x = hashWithSalt s $ _func (x :: MLILFunction)

data MLILSSAFunction = MLILSSAFunction
  { _handle :: BNMediumLevelILFunction
  , _func :: Function
  } deriving (Ord, Show)

instance Eq MLILSSAFunction where
  (==) a b = _func (a :: MLILSSAFunction) == _func (b :: MLILSSAFunction)

instance Hashable MLILSSAFunction where
  hashWithSalt s x = hashWithSalt s $ _func (x :: MLILSSAFunction)


$(makeFieldsNoPrefix ''Function)
$(makeFieldsNoPrefix ''LLILFunction)
$(makeFieldsNoPrefix ''LLILSSAFunction)
$(makeFieldsNoPrefix ''MLILFunction)
$(makeFieldsNoPrefix ''MLILSSAFunction)
