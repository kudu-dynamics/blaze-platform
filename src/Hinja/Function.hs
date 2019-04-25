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

module Hinja.Function where

import Hinja.Prelude hiding (onException)
import qualified Prelude as P
import qualified Data.Text as Text
import qualified Control.Exception as E
import qualified Hinja.C.Main as BN
import Hinja.C.Pointers ( BNFunction
                        , BNMediumLevelILFunction
                        , BNBinaryView)
import Control.Lens.TH

data Function = Function
  { _handle :: BNFunction
  , _name :: Text
  , _start :: Word64
  } deriving (Eq, Ord, Show)

data MLILFunction = MLILFunction
  { _handle :: BNMediumLevelILFunction
  , _func :: Function
  } deriving (Eq, Ord, Show)

data MLILSSAFunction = MLILSSAFunction
  { _handle :: BNMediumLevelILFunction
  , _func :: Function
  } deriving (Eq, Ord, Show)

$(makeFields ''Function)
$(makeFields ''MLILFunction)
$(makeFields ''MLILSSAFunction)

createFunction :: BNFunction -> IO Function
createFunction ptr = Function ptr
                     <$> (Text.pack <$> BN.getFunctionName ptr)
                     <*> BN.getFunctionStart ptr

getFunctions :: BNBinaryView -> IO [Function]
getFunctions bv = BN.getFunctions bv >>= traverse createFunction
