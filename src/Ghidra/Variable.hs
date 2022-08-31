{-# LANGUAGE DataKinds #-}
module Ghidra.Variable
  ( module Ghidra.Variable
  , VarType
  , VarNode
  , HighVariable
  , HighVariableType
  , DataType
  ) where

import Ghidra.Prelude hiding (toList, Const(Const), DataType, mkDataType)

import Foreign.JNI.Types (JObject)
import qualified Language.Java as Java
import qualified Ghidra.Types as J
import Ghidra.Types.Variable
import Ghidra.Address (mkAddress)
import qualified Data.Text as Text

mkVarType :: J.VarNode -> IO VarType
mkVarType v = Java.call v "isConstant" >>= \case
  True -> do
    -- value of const is stored in address
    addr :: J.Address <- Java.call v "getAddress"
    Const <$> Java.call addr "getOffset"
  False -> do
    Addr <$> (Java.call v "getAddress" >>= mkAddress)

mkVarNode :: J.VarNode -> IO VarNode
mkVarNode v = do
  sz :: Int32 <- Java.call v "getSize"
  VarNode <$> mkVarType v <*> pure (fromIntegral sz)

mkHighVariableType :: J.HighVariable -> IO HighVariableType
mkHighVariableType hv = do
  cls :: J.Class <- Java.call (coerce hv :: JObject) "getClass"
  name :: Text <- Java.call cls "getName" >>= Java.reify
  case lastMay $ Text.splitOn "." name of
    Nothing -> error "Class name invalid"
    Just cname -> case cname of
      "HighConstant" -> do
        s :: J.Scalar <- Java.call (coerce hv :: J.HighConstant) "getScalar"
        HighConstant <$> Java.call s "getValue"
      "HighGlobal" -> return HighGlobal
      "HighLocal" -> return HighLocal
      "HighOther" -> return HighOther
      other -> error $ "Invalid class name: " <> cs other

mkDataType :: J.HighVariable -> IO DataType
mkDataType hv = do
  dt :: J.DataType <- Java.call hv "getDataType"
  DataType <$> (Java.call dt "getName" >>= Java.reify)

mkHighVariable :: J.HighVariable -> IO HighVariable
mkHighVariable hv = do
  sym' :: J.HighSymbol <- Java.call hv "getSymbol"
  symName <- Java.call sym' "getName" >>= Java.reify
  sz :: Int32 <- Java.call hv "getSize"
  dt <- mkDataType hv
  hvt <- mkHighVariableType hv
  return $ HighVariable
    { dataType = dt
    , symbol = symName
    , size = fromIntegral sz
    , highVariableType = hvt
    }

