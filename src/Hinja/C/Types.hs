{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Hinja.C.Types where

import Hinja.Prelude

import Foreign.C.Types
import Foreign.ForeignPtr
import Hinja.C.Enums (BNMediumLevelILOperation(..))

type List = Ptr

newtype Address = Address Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype BNPointer = BNPointer (ForeignPtr BNPointer)

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

