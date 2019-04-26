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

getMLILExprIndex :: MLILFunction -> InstructionIndex MLILFunction
                    -> IO (ExpressionIndex MLILFunction)
getMLILExprIndex fn iindex =
  BN.getMediumLevelILIndexForInstruction (fn ^. Func.handle) (coerceInstructionIndex iindex)

getMLILSSAExprIndex :: MLILSSAFunction -> InstructionIndex MLILSSAFunction
                    -> IO (ExpressionIndex MLILSSAFunction)
getMLILSSAExprIndex fn iindex = 
  BN.getMediumLevelILIndexForInstruction fnPtr (coerceInstructionIndex iindex)
  >>= BN.getMediumLevelILSSAExprIndex fnPtr
  where
    fnPtr = fn ^. Func.handle

