{-# LANGUAGE DataKinds #-}
module Ghidra.Variable
  ( module Ghidra.Variable
  , DataType
  , HighVarNode
  , HighVariable
  , HighVariableType
  , VarNode
  , VarType(..)
  ) where

import Ghidra.Prelude hiding (toList, Const(Const), DataType, mkDataType)

import Foreign.JNI.Types (JObject)
import qualified Language.Java as Java
import qualified Ghidra.Types as J
import Ghidra.Types.Variable
import Ghidra.Address (mkAddress)
import Ghidra.Util (maybeNullCall, maybeNull, isJNull)
import qualified Data.Text as Text
import qualified Foreign.JNI as JNI


mkVarType :: J.VarNode -> IO VarType
mkVarType v = Java.call v "isConstant" >>= \case
  True -> do
    -- value of const is stored in address
    addr :: J.Address <- Java.call v "getAddress"
    Const <$> Java.call addr "getOffset"
  False -> do
    Addr <$> (Java.call v "getAddress" >>= JNI.newGlobalRef >>= mkAddress)

mkVarNode :: J.VarNode -> IO VarNode
mkVarNode v = do
  sz :: Int32 <- Java.call v "getSize"
  VarNode <$> mkVarType v <*> pure (fromIntegral sz)

mkHighVariableType :: J.HighVariable -> IO HighVariableType
mkHighVariableType hv = do
  cls :: J.Class <- Java.call (coerce hv :: JObject) "getClass" >>= JNI.newGlobalRef
  name :: Text <- Java.call cls "getName"
                  >>= JNI.newGlobalRef
                  >>= Java.reify
  case lastMay $ Text.splitOn "." name of
    Nothing -> error "Class name invalid"
    Just cname -> case cname of
      "HighConstant" -> do
        s :: J.Scalar <- Java.call (coerce hv :: J.HighConstant) "getScalar"
                  >>= JNI.newGlobalRef
        HighConstant <$> Java.call s "getValue"
      "HighGlobal" -> return HighGlobal
      "HighLocal" -> return HighLocal
      "HighParam" -> do
        slot :: Int32 <- Java.call (coerce hv :: J.HighParam) "getSlot"
        return . HighParam . fromIntegral $ slot
      "HighOther" -> return HighOther
      other -> error $ "Invalid class name: " <> cs other

mkDataType :: J.HighVariable -> IO DataType
mkDataType hv = do
  dt :: J.DataType <- Java.call hv "getDataType" >>= JNI.newGlobalRef
  DataType <$> (Java.call dt "getName" >>= Java.reify)

mkHighVariable :: J.HighVariable -> IO HighVariable
mkHighVariable hv = do
  putText $ "getting size " <> show hv
  sz :: Int32 <- Java.call hv "getSize"
  putText "got size"
  mVarNameStr <- maybeNullCall $ Java.call hv "getName" >>= Java.reify
  putText "got name"
  dt <- mkDataType hv
  hvt <- mkHighVariableType hv
  return $ HighVariable
    { dataType = dt
    , name = mVarNameStr
    , size = fromIntegral sz
    , highVariableType = hvt
    }

mkHighVarNode :: J.VarNodeAST -> IO HighVarNode
mkHighVarNode v = do
  -- sz :: Int32 <- Java.call (coerce v :: J.VarNode)  "getSize"
  putText $ "mkHighVarNode " <> show v
  sz :: Int32 <- Java.call v "getSize"
  putText $ "got size " <> show sz
  mhv <- maybeNull <$> Java.call v "getHigh"
  putText $ "Got high: " <> show mhv <> " " <> (show $ isJNull <$> mhv)
  mhv' <- maybe (return Nothing) (fmap Just . (mkHighVariable <=< JNI.newGlobalRef)) mhv
  HighVarNode <$> mkVarType (coerce v) <*> pure (fromIntegral sz) <*> pure mhv'

