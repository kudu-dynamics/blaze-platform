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

data Expr = NOP
          | SET_VAR SetVarOp
          | SET_VAR_FIELD SetVarFieldOp
          deriving (Eq, Ord, Show)

data SetVarOp = SetVarOp { _dest :: Variable
                         , _src :: Expr
                         } deriving (Eq, Ord, Show)

data SetVarFieldOp = SetVarFieldOp { _dest :: Variable
                                   , _offset :: Int
                                   , _src :: Expr
                                   } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''SetVarOp)
$(makeFieldsNoPrefix ''SetVarFieldOp)

