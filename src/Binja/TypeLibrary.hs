{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Binja.TypeLibrary where

import Binja.C.Helpers (getFunctionParameterVariables')
import qualified Binja.Core as Bn
import qualified Binja.Function as Bf
import Binja.Prelude hiding (handle)
import Binja.Types.Function
import Binja.Types.TypeLibrary as Exports
import Binja.Types.Variable
import Binja.Variable (getVarType)
import qualified Data.Text as Text

functionTsFromTypeLib :: Bn.BNTypeLibrary -> IO [FunctionT]
functionTsFromTypeLib tl = Bn.getTypeLibraryNamedObjects tl >>= traverse f
  where
    f :: BNQualifiedNameAndType -> IO FunctionT
    f = \case
      BNQualifiedNameAndType n _ _ (Just t) -> do
        let fName = getName n
        returnType <- bnTypeToVarType t
        args <- Bn.getTypeParameters t >>= traverse paramToVarType
        return $ FunctionT fName returnType args
      BNQualifiedNameAndType n _ _ Nothing -> return $ FunctionT (getName n) Nothing [Nothing]

    bnTypeToVarType :: Bn.BNType -> IO (Maybe VarType)
    bnTypeToVarType v = Bn.getChildType v >>= getVarType
    getName :: [Text] -> Text
    getName l = maybe (Text.pack "") identity $ headMay l    
    paramToVarType :: BNFunctionParameter -> IO (Maybe VarType)
    paramToVarType = \case
      BNFunctionParameter _ (Just fpT) _ _ _ _ _ -> bnTypeToVarType fpT
      BNFunctionParameter _ Nothing _ _ _ _ _ -> return Nothing
