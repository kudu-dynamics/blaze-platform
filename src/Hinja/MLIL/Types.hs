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
import Hinja.C.Pointers

data MediumLevelILInstruction = MediumLevelILInstruction
  { _operation :: BNMediumLevelILOperation
  , _sourceOperand :: Bool
  , _size :: OperationSize
  , _operands :: OperandsData
  , _address :: Address
  } deriving (Eq, Ord, Show)

newtype OpIndex = OpIndex Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype OperationSize = OperationSize Word64
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype OperandsData = OperandsData [Word64]
  deriving (Eq, Ord, Show)

data OpBuilderCtx fun = OpBuilderCtx
  { _func :: fun
  , _exprIndex :: ExpressionIndex fun
  , _opData :: OperandsData
  , _size :: OperationSize
  } deriving (Eq, Ord, Show)

class ( HasHandle fun BNMediumLevelILFunction
      , HasFunc fun Function ) => StatementFunction fun where
  getExprIndex :: fun -> InstructionIndex fun -> IO (ExpressionIndex fun)

newtype OpBuilder fun a = OpBuilder
  { runOpBuilder_ :: ReaderT (OpBuilderCtx fun) (StateT OpIndex IO) a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadReader (OpBuilderCtx fun)
           , MonadState OpIndex)

runOpBuilder :: OpBuilder t a -> (OpBuilderCtx t, OpIndex) -> IO a
runOpBuilder m (ctx, s) = flip evalStateT s . flip runReaderT ctx . runOpBuilder_ $ m

-- takes first op data long and reduces state
-- crashes if op-data is empty -- should never happen
takeOpDataWord :: OpBuilder t Word64
takeOpDataWord = do
  (OpIndex n) <- get
  if n >= 5
    then P.error "takeOpData: exhausted OperandsData"
    else do
      (OperandsData xs) <- _opData <$> ask
      modify (+1)
      return $ xs !! fromIntegral n

getAndAdvanceOpIndex :: OpBuilder t OpIndex
getAndAdvanceOpIndex = do
  n <- get
  modify (+1)
  return n

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

data SSAVariableDestAndSrc = SSAVariableDestAndSrc
  { _dest :: SSAVariable
  , _src :: SSAVariable
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
$(makeFieldsNoPrefix ''SSAVariable)
$(makeFieldsNoPrefix ''SSAVariableDestAndSrc)
$(makeFieldsNoPrefix ''SetVarSSAOp)
$(makeFieldsNoPrefix ''SetVarOp)
$(makeFieldsNoPrefix ''SetVarFieldOp)

