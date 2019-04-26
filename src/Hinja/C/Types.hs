{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Hinja.C.Types where

import Hinja.Prelude

import Foreign.ForeignPtr

type List = Ptr

newtype Address = Address Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)


data IL
data LLIL
data MLIL
data MLILSSA

newtype InstructionIndex x = InstructionIndex Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype ExpressionIndex x = ExpressionIndex Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

coerceInstructionIndex :: InstructionIndex a -> InstructionIndex b
coerceInstructionIndex (InstructionIndex x) = InstructionIndex x

coerceExpressionIndex :: ExpressionIndex a -> ExpressionIndex b
coerceExpressionIndex (ExpressionIndex x) = ExpressionIndex x


class HasFinalizer a where
  finalizer :: FinalizerPtr a
  
class Pointer a where
  pointerWrap :: ForeignPtr a -> a
  pointerUnwrap :: a -> ForeignPtr a
  pointerFinalizer :: Maybe (FinalizerPtr a)
