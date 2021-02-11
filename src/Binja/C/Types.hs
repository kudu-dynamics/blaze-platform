{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Binja.C.Types where

import Binja.Prelude

import Foreign.ForeignPtr

type List = Ptr

newtype BNPointer = BNPointer (ForeignPtr BNPointer)

newtype InstructionIndex x = InstructionIndex Word64
  deriving (Eq, Ord, Show, Enum, Functor, Foldable, Traversable, Generic)
  deriving newtype (Num, Real, Integral)

instance Hashable (InstructionIndex x)

newtype ExpressionIndex x = ExpressionIndex Word64
  deriving (Eq, Ord, Show, Enum, Functor, Foldable, Traversable, Generic)
  deriving newtype (Num, Real, Integral)

instance Hashable (ExpressionIndex x)

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

