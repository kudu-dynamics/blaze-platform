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

module Binja.C.Types where

import Binja.Prelude

import Foreign.ForeignPtr

type List = Ptr

newtype Address = Address Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral, Generic)

instance Hashable Address

newtype BNPointer = BNPointer (ForeignPtr BNPointer)

newtype InstructionIndex x = InstructionIndex Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral, Functor, Foldable, Traversable)

newtype ExpressionIndex x = ExpressionIndex Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral, Functor, Foldable, Traversable)

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

