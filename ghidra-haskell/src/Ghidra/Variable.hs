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
import Ghidra.Types.Internal (Ghidra, runIO)
import Ghidra.Types.Variable
import Ghidra.Address (Address, mkAddress)
import Ghidra.Util (maybeNullCall, maybeNull)
import qualified Data.Text as Text
import qualified Foreign.JNI as JNI


mkVarType :: J.VarNode -> Ghidra VarType
mkVarType v = runIO (Java.call v "isConstant") >>= \case
  True -> do
    -- value of const is stored in address
    addr :: J.Address <- runIO $ Java.call v "getAddress"
    Const <$> runIO (Java.call addr "getOffset")
  False -> do
    Addr <$> (runIO (Java.call v "getAddress" >>= JNI.newGlobalRef) >>= mkAddress)

mkVarNode :: J.VarNode -> Ghidra VarNode
mkVarNode v = do
  sz :: Int32 <- runIO $ Java.call v "getSize"
  VarNode <$> mkVarType v <*> pure (fromIntegral sz)

mkHighVariableType :: J.HighVariable -> Ghidra HighVariableType
mkHighVariableType hv = do
  cls :: J.Class <- runIO $ Java.call (coerce hv :: JObject) "getClass" >>= JNI.newGlobalRef
  name' :: Text <- runIO $ Java.call cls "getName"
                  >>= JNI.newGlobalRef
                  >>= Java.reify
  case lastMay $ Text.splitOn "." name' of
    Nothing -> error "Class name invalid"
    Just cname -> case cname of
      "HighConstant" -> do
        s :: J.Scalar <- runIO $ Java.call (coerce hv :: J.HighConstant) "getScalar"
                  >>= JNI.newGlobalRef
        HighConstant <$> runIO (Java.call s "getValue")
      "HighGlobal" -> return HighGlobal
      "HighLocal" -> return HighLocal
      "HighParam" -> do
        slot :: Int32 <- runIO $ Java.call (coerce hv :: J.HighParam) "getSlot"
        return . HighParam . fromIntegral $ slot
      "HighOther" -> return HighOther
      other -> error $ "Invalid class name: " <> cs other

mkDataType :: J.HighVariable -> Ghidra DataType
mkDataType hv = runIO $ do
  dt :: J.DataType <- Java.call hv "getDataType" >>= JNI.newGlobalRef
  DataType <$> (Java.call dt "getName" >>= Java.reify)

mkHighVariable :: J.HighVariable -> Ghidra HighVariable
mkHighVariable hv = do
  sz :: Int32 <- runIO $ Java.call hv "getSize"
  mVarNameStr <- maybeNullCall . runIO $ Java.call hv "getName" >>= Java.reify
  dt <- mkDataType hv
  hvt <- mkHighVariableType hv
  return $ HighVariable
    { dataType = dt
    , name = mVarNameStr
    , size = fromIntegral sz
    , highVariableType = hvt
    }

mkHighVarNode :: J.VarNodeAST -> Ghidra HighVarNode
mkHighVarNode v = do
  -- sz :: Int32 <- Java.call (coerce v :: J.VarNode)  "getSize"
  sz :: Int32 <- runIO $ Java.call v "getSize"
  mhv <- maybeNull <$> runIO (Java.call v "getHigh")
  mhv' <- maybe (return Nothing) (fmap Just . (mkHighVariable <=< runIO . JNI.newGlobalRef)) mhv
  HighVarNode <$> mkVarType (coerce v) <*> pure (fromIntegral sz) <*> getPcAddress <*> pure mhv'

  where
    -- Only works for high varnodes
    -- "getPCAddress" causes nullPointerException if used on low varnode
    getPcAddress :: Ghidra (Maybe Address)
    getPcAddress = do
      addr :: J.Address <- runIO $ Java.call v "getPCAddress"
      noAddress :: J.Address <- runIO $ Java.getStaticField "ghidra.program.model.address.Address" "NO_ADDRESS"
      if addr == noAddress
        then return Nothing
        else fmap Just $ runIO (JNI.newGlobalRef addr) >>= mkAddress
