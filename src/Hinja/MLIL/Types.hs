{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Hinja.MLIL.Types where

import Hinja.Prelude hiding (onException, handle)

import qualified Prelude as P
import Hinja.Types
import Hinja.C.Enums
import Hinja.C.Types


data MediumLevelILInstruction = MediumLevelILInstruction
  { _operation :: BNMediumLevelILOperation
  , _sourceOperand :: Bool
  , _size :: OperationSize
  , _operands :: OperandsData
  , _address :: Address
  } deriving (Eq, Ord, Show)

newtype OperationSize = OperationSize Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype OperandsData = OperandsData [Word64]
  deriving (Eq, Ord, Show)

data OpBuilderCtx fun = OpBuilderCtx
  { _func :: fun
  , _exprIndex :: ExpressionIndex fun
  , _opIndex :: Int
  , _size :: OperationSize
  } deriving (Eq, Ord, Show)

newtype OpBuilder fun a = OpBuilder
  { runOpBuilder_ :: ReaderT (OpBuilderCtx fun) (StateT OperandsData IO) a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadReader (OpBuilderCtx fun)
           , MonadState OperandsData)

runOpBuilder :: OpBuilder t a -> (OpBuilderCtx t, OperandsData) -> IO a
runOpBuilder m (ctx, s) = flip evalStateT s . flip runReaderT ctx . runOpBuilder_ $ m

-- takes first op data long and reduces state
-- crashes if op-data is empty -- should never happen
takeOpDataWord :: OpBuilder t Word64
takeOpDataWord = do
  (OperandsData ds) <- get
  case ds of
    [] -> P.error "takeOpData: exhausted OperandsData"
    (x:xs) -> put (OperandsData xs) >> return x

data Instruction t = Instruction
  { _address :: Address
  , _index :: InstructionIndex t
  , _size :: OperationSize
  , _op :: Operation t
  } deriving (Eq, Ord, Show)

data Expression t = Expression
  { _address :: Address
  , _index :: ExpressionIndex t
  , _size :: OperationSize
  , _op :: Operation t
  } deriving (Eq, Ord, Show)


data Operation t = NOP
                 | SET_VAR (SetVarOp t)
                 | SET_VAR_FIELD (SetVarFieldOp t)
                 | SET_VAR_SSA (SetVarSSAOp t)
                 deriving (Eq, Ord, Show)

data SSAVariable = SSAVariable { _var :: Variable
                               , _version :: Int
                               } deriving (Eq, Ord, Show)

data SetVarSSAOp t = SetVarSSAOp { _dest :: SSAVariable
                                 , _src  :: Expression t
                                 } deriving (Eq, Ord, Show)

data SetVarOp t = SetVarOp { _dest :: Variable
                           , _src :: Expression t
                           } deriving (Eq, Ord, Show)

data SetVarFieldOp t = SetVarFieldOp { _dest :: Variable
                                     , _offset :: Int
                                     , _src :: Expression t
                                     } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''MediumLevelILInstruction)
$(makeFieldsNoPrefix ''OpBuilderCtx)
$(makeFieldsNoPrefix ''Expression)
$(makeFieldsNoPrefix ''Instruction)
$(makeFieldsNoPrefix ''SetVarSSAOp)
$(makeFieldsNoPrefix ''SetVarOp)
$(makeFieldsNoPrefix ''SetVarFieldOp)

