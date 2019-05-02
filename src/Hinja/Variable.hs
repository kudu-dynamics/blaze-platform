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

module Hinja.Variable
  ( module Exports
  , getVariableFromIdentifier
  ) where

import Hinja.Prelude hiding (onException, handle)
import qualified Data.Text as Text
import qualified Hinja.C.Main as BN
import Hinja.C.Pointers
import Hinja.Types
import Hinja.C.Structs
import Hinja.C.Enums
import qualified Hinja.Function as Func
import Hinja.Types as Exports ( Variable(..)
                              , VarType(..)
                              , index
                              , storage
                              , sourceType
                              , varType
                              , name
                              , confidence
                              , typeClass
                              , width
                              , alignment
                              , signed
                              , signedConfidence
                              , isConst
                              , constConfidence )


---------- Variables

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
