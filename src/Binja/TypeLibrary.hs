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

functionTsFromTypeLib :: Bn.BNTypeLibrary -> IO [FunctionT]
functionTsFromTypeLib tl = Bn.getTypeLibraryNamedObjects tl >>= traverse f
  where
    f :: BNQualifiedNameAndType -> IO FunctionT
    f x = maybe 
          (return $ FunctionT (getName $ x ^. name) Nothing [Nothing])
          (\t -> createFunctionType (getName $ x ^. name) t)
          $ x ^. qnType

    createFunctionType :: Text -> Bn.BNType -> IO FunctionT
    createFunctionType name' type' = do
      return' <- bnTypeToVarType type'
      args <- Bn.getTypeParameters type' >>= traverse paramToVarType
      return $ FunctionT name' return' args
    bnTypeToVarType :: Bn.BNType -> IO (Maybe VarType)
    bnTypeToVarType v = Bn.getChildType v >>= getVarType
    getName :: [Text] -> Text
    getName l = maybe (Text.pack "") identity $ headMay l    
    paramToVarType :: BNFunctionParameter -> IO (Maybe VarType)
    paramToVarType p = maybe (return Nothing) bnTypeToVarType $ p ^. fpType

