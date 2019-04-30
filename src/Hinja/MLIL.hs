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

module Hinja.MLIL
  ( module Exports
  , getExprIndex
  , getMediumLevelILInstructionByExpressionIndex
  , getMediumLevelILInstructionByInstructionIndex
  , expression
  , instruction
  ) where

import Hinja.Prelude hiding (onException, handle)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Hinja.C.Main as BN
import Hinja.C.Pointers
import Hinja.Types
import Hinja.Function ( Function
                      , LLILFunction
                      , MLILFunction
                      , MLILSSAFunction
                      , createFunction
                      )
import qualified Hinja.Function as Func
import Hinja.MLIL.Types as Exports
import Hinja.C.Types
import Hinja.MLIL.Types
import qualified Hinja.C.Enums as BN


class HasHandle fun BNMediumLevelILFunction => StatementFunction fun where
  getExprIndex :: fun -> InstructionIndex fun -> IO (ExpressionIndex fun)
  
instance StatementFunction MLILFunction where
  getExprIndex fn iindex = BN.getMediumLevelILIndexForInstruction
    (fn ^. Func.handle) (coerceInstructionIndex iindex)

instance StatementFunction MLILSSAFunction where
  getExprIndex fn iindex =
    BN.getMediumLevelILIndexForInstruction fnPtr (coerceInstructionIndex iindex)
    >>= BN.getMediumLevelILSSAExprIndex fnPtr
    where
      fnPtr = fn ^. Func.handle


getMediumLevelILInstructionByExpressionIndex :: StatementFunction fun
                         => fun -> ExpressionIndex fun
                         -> IO (MediumLevelILInstruction)
getMediumLevelILInstructionByExpressionIndex fn eindex =
  BN.getMediumLevelILByIndex (fn ^. Func.handle) (coerceExpressionIndex eindex)

getMediumLevelILInstructionByInstructionIndex :: StatementFunction fun
                          => fun -> InstructionIndex fun
                          -> IO (MediumLevelILInstruction) 
getMediumLevelILInstructionByInstructionIndex fn iindex = getExprIndex fn iindex >>= getMediumLevelILInstructionByExpressionIndex fn

buildVariable :: OpBuilder t Variable
buildVariable = undefined

buildSSAVariable :: OpBuilder t SSAVariable
buildSSAVariable = do
  v <- buildVariable
  version <- fromIntegral <$> takeOpDataWord
  return $ SSAVariable { _var = v
                       , _version = version }

buildExpr :: StatementFunction t => OpBuilder t (Expression t)
buildExpr = do
  w <- takeOpDataWord
  fn <- (view func) <$> ask
  liftIO $ expression fn (fromIntegral w)

expression :: StatementFunction fun => fun -> ExpressionIndex fun -> IO (Expression fun)
expression fn eindex = do
  (mlil, op') <- getOperation fn eindex
  return $ Expression { _address = mlil ^. address
                      , _index = eindex
                      , _size = mlil ^. size
                      , _op = op' }

instruction :: StatementFunction fun => fun -> InstructionIndex fun -> IO (Instruction fun)
instruction fn iindex = do
  eindex <- getExprIndex fn iindex
  (mlil, op') <- getOperation fn eindex
  return $ Instruction { _address = mlil ^. address
                       , _index = iindex
                       , _size = mlil ^. size
                       , _op = op' }

getOperation :: StatementFunction fun
            => fun -> ExpressionIndex fun -> IO (MediumLevelILInstruction, Operation fun)
getOperation fn eindex = do
  mlil <- getMediumLevelILInstructionByExpressionIndex fn eindex
  let ctx = OpBuilderCtx { _func = fn
                         , _exprIndex = eindex
                         , _opIndex = 0
                         , _size = mlil ^. size }
      st = mlil ^. operands
  fmap (mlil,) . flip runOpBuilder (ctx, st) $ case mlil ^. operation of
    BN.MLIL_SET_VAR_SSA ->
      fmap SET_VAR_SSA . SetVarSSAOp <$> buildSSAVariable <*> buildExpr
    _ -> undefined


