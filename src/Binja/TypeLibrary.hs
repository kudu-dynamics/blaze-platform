{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Binja.TypeLibrary where

import qualified Binja.Core as Bn
import Binja.Prelude hiding (handle)
import Binja.Types.TypeLibrary as Exports
import Binja.Types.Variable hiding (name)
import Binja.Variable (getVarType)
import qualified Data.Text as Text

functionTsFromTypeLib :: Bn.BNTypeLibrary -> IO [FunctionType]
functionTsFromTypeLib tl = Bn.getTypeLibraryNamedObjects tl >>= traverse f
  where
    f :: BNQualifiedNameAndType -> IO FunctionType
    f x = maybe 
          (return $ FunctionType (getName $ x ^. name) Nothing [Nothing])
          (\t -> createFunctionType (getName $ x ^. name) t)
          $ x ^. bnTypePtr

    createFunctionType :: Text -> Bn.BNType -> IO FunctionType
    createFunctionType name' type' = do
      return' <- bnTypeToVarType type'
      args <- Bn.getTypeParameters type' >>= traverse paramToVarType
      return $ FunctionType name' return' args
    bnTypeToVarType :: Bn.BNType -> IO (Maybe VarType)
    bnTypeToVarType v = Bn.getChildType v >>= getVarType
    getName :: [Text] -> Text
    getName l = maybe (Text.pack "") identity $ headMay l    
    paramToVarType :: BNFunctionParameter -> IO (Maybe VarType)
    paramToVarType p = maybe (return Nothing) bnTypeToVarType $ p ^. bnTypePtr