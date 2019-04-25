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

module Hinja.Function where

import Hinja.Prelude hiding (onException, handle)
import qualified Data.Text as Text
import qualified Hinja.C.Main as BN
import Hinja.C.Pointers ( BNFunction
                        , BNMediumLevelILFunction
                        , BNBinaryView)

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

$(makeFieldsNoPrefix ''Function)
$(makeFieldsNoPrefix ''MLILFunction)
$(makeFieldsNoPrefix ''MLILSSAFunction)

createFunction :: BNFunction -> IO Function
createFunction ptr = Function ptr
                     <$> (Text.pack <$> BN.getFunctionName ptr)
                     <*> BN.getFunctionStart ptr

getFunctions :: BNBinaryView -> IO [Function]
getFunctions bv = BN.getFunctions bv >>= traverse createFunction

getMLILFunction :: Function -> IO MLILFunction
getMLILFunction fn = MLILFunction
  <$> BN.getFunctionMediumLevelIL (fn ^. handle)
  <*> pure fn

getMLILSSAFunction :: Function -> IO MLILSSAFunction
getMLILSSAFunction fn = MLILSSAFunction
  <$> (BN.getFunctionMediumLevelIL (fn ^. handle)  >>= BN.getMediumLevelILSSAForm)
  <*> pure fn
