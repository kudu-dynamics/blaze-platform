{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Binja.TypeLibrary
  ( module Binja.TypeLibrary
  , module Exports
  ) where

import qualified Binja.Core as BN
import Binja.Prelude hiding (handle)
import Binja.Types.TypeLibrary as Exports
import Binja.Types.Variable hiding (name)
import Binja.Variable (getVarType')

-- | gets the types for all functions in the type lib
-- if there are non-function types in the typelib, they will be ignored
getFunctionTypes :: BN.BNTypeLibrary -> IO [FunctionType]
getFunctionTypes tl = BN.getTypeLibraryNamedObjects tl >>= mapMaybeM f
  where
    getFromMaybe :: Monad m => Maybe a -> MaybeT m a
    getFromMaybe = maybe mzero return

    f :: BNQualifiedNameAndType -> IO (Maybe FunctionType)
    f x = runMaybeT $ do
      bnt <- getFromMaybe $ x ^. bnTypePtr
      retType <- liftIO $ getVarType' bnt 255
      params <- liftIO $ BN.getTypeParameters bnt
      paramsTypes <- traverse paramToVarType params

      name' <- getFromMaybe . lastMay $ x ^. name

      return $ FunctionType name' (x ^. name) retType paramsTypes

    paramToVarType :: BNFunctionParameter -> MaybeT IO VarType
    paramToVarType p = do
      bnt <- getFromMaybe $ p ^. bnTypePtr
      liftIO . getVarType' bnt $ p ^. typeConfidence
