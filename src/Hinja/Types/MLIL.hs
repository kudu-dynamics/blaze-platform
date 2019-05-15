{-# LANGUAGE TemplateHaskell #-}

module Hinja.Types.MLIL where

import Hinja.Prelude

import Hinja.C.Enums
import Hinja.C.Types

data MediumLevelILInstruction = MediumLevelILInstruction
  { _operation :: BNMediumLevelILOperation
  , _sourceOperand :: Bool
  , _size :: OperationSize
  , _operands :: OperandsData
  , _address :: Address
  } deriving (Eq, Ord, Show)

newtype Intrinsic = Intrinsic Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype OpIndex = OpIndex Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype OperationSize = OperationSize Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype OperandsData = OperandsData [Word64]
  deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''MediumLevelILInstruction)
