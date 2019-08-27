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

module Binja.Variable
  ( module Exports
  , getVariableFromIdentifier
  ) where

import           Binja.Prelude                   hiding ( handle )

import qualified Data.Text            as Text
import qualified Binja.C.Main         as BN
import qualified Binja.Function       as Func
import           Binja.Types.Function
import           Binja.Types.Variable as Exports

getVariableFromIdentifier :: Function -> VariableIdentifier -> IO Variable
getVariableFromIdentifier fn vid = do
  bnvar <- BN.fromVariableIdentifier vid
  vname <- BN.getVariableName fptr bnvar
  vtc <- BN.getVariableType fptr bnvar
  mvt <- case (vtc ^. bnType) of
    Nothing -> return Nothing
    Just t -> do
      signedc <- BN.isTypeSigned t
      constc <- BN.isTypeConst t
      tclass <- BN.getTypeClass t
      talignment <- BN.getTypeAlignment t
      twidth <- BN.getTypeWidth t
      return . Just $ VarType { _confidence = vtc ^. confidence
                              , _typeClass = tclass
                              , _width = twidth
                              , _alignment = talignment
                              , _signed = signedc ^. value
                              , _signedConfidence = signedc ^. confidence
                              , _isConst = constc ^. value
                              , _constConfidence = constc ^. confidence
                              }
  return $ Variable { _index = bnvar ^. index
                    , _name = Text.pack vname
                    , _storage = bnvar ^. storage
                    , _sourceType = bnvar ^. sourceType
                    , _varType = mvt
                    }
  where
    fptr = fn ^. Func.handle
