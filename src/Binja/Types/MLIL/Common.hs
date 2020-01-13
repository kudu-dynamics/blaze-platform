{-# LANGUAGE TemplateHaskell #-}

module Binja.Types.MLIL.Common where

import Binja.Prelude

import Binja.Types.Variable (Variable)

data SSAVariable = SSAVariable { _var :: Variable
                               , _version :: Int
                               } deriving (Eq, Ord, Show, Generic)

instance Hashable SSAVariable

data SSAVariableDestAndSrc = SSAVariableDestAndSrc
  { _dest :: SSAVariable
  , _src :: SSAVariable
  } deriving (Eq, Ord, Show, Generic)

instance Hashable SSAVariableDestAndSrc

newtype Intrinsic = Intrinsic Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral, Generic)

instance Hashable Intrinsic