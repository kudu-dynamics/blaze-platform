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

module Hinja.MLIL where

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
import Hinja.C.Types
import Hinja.MLIL.Types


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


getMLILByExpressionIndex :: StatementFunction fun
                         => fun -> ExpressionIndex fun
                         -> IO (Ptr BNMediumLevelILInstruction)
getMLILByExpressionIndex fn eindex =
  BN.getMediumLevelILByIndex' (fn ^. Func.handle) (coerceExpressionIndex eindex)

getMLILByInstructionIndex :: StatementFunction fun
                          => fun -> InstructionIndex fun
                          -> IO (Ptr BNMediumLevelILInstruction) 
getMLILByInstructionIndex fn iindex = getExprIndex fn iindex >>= getMLILByExpressionIndex fn
