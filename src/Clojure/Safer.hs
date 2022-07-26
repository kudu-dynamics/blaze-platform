{-# LANGUAGE DataKinds #-}
module Clojure.Safer where

import Prelude
-- import qualified Clojure.JNI as JNI
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Vector (Vector)
import qualified Data.Vector as V
import Foreign
import Foreign.C.Types
import qualified Foreign.JNI as JNI
import qualified Foreign.JNI.Types as JNIT
import Foreign.JNI.Types (JClass, JMethodID, JObject, MethodSignature)
import GHC.Generics (Generic)
import Prelude.Singletons (Sing, SingI(sing), SomeSing(..))


clojureOpts :: [ByteString]
clojureOpts = ["-Djava.class.path=res/clojure/clojure-1.11.1.jar:res/clojure/spec.alpha-0.3.218.jar"]


data ClojureCtx = ClojureCtx
  { clojureClass :: JClass
  , readMethod :: JMethodID
  , varMethod :: JMethodID
  , varQualMethod :: JMethodID
  , ifnClass :: JClass
  , invokeMethods :: Vector JMethodID

  , longClass :: JClass
  , longValueMethod :: JMethodID
  , longInitMethod :: JMethodID

  , objectClass :: JClass
  , toStringMethod :: JMethodID
  } deriving (Show, Generic)


-- | Run this in bounded IO
initClojureCtx_ :: IO ClojureCtx
initClojureCtx_ = do
  let objArg = SomeSing (sing :: Sing ('JNIT.Class "java/lang/Object"))
      strArg = SomeSing (sing :: Sing ('JNIT.Class "java/lang/String"))
      longArg = SomeSing (sing :: Sing ('JNIT.Prim "long"))

      objRet = JNIT.SClass "java/lang/Object"
      ifnRet = JNIT.SClass "clojure/lang/IFn"
      longRet = JNIT.SPrim "long"
      voidRet = JNIT.SVoid
      strRet = JNIT.SClass "java/lang/String"
    
  clojureClass' <- (JNI.findClass . JNIT.referenceTypeName $ JNIT.SClass "clojure/java/api/Clojure") >>= mkGlobal
  
  let readMethodSig = JNIT.methodSignature [strArg] objRet
  readMethod' <- JNI.getStaticMethodID clojureClass' "read" readMethodSig

  let varMethodSig = JNIT.methodSignature [objArg] ifnRet
  varMethod' <- JNI.getStaticMethodID clojureClass' "var" varMethodSig

  let varQualMethodSig = JNIT.methodSignature [objArg, objArg] ifnRet
  varQualMethod' <- JNI.getStaticMethodID clojureClass' "var" varMethodSig

  ifnClass' <- (JNI.findClass . JNIT.referenceTypeName $ JNIT.SClass "clojure/lang/IFn") >>= mkGlobal

  invokeFns <- V.forM (V.fromListN 20 [0..]) $ \n -> do
    let invokeSig = JNIT.methodSignature (replicate n objArg) objRet
    JNI.getMethodID ifnClass' "invoke" invokeSig

  longClass' <- (JNI.findClass . JNIT.referenceTypeName $ JNIT.SClass "java/lang/Long") >>= mkGlobal
  longValueMethod <- JNI.getMethodID longClass' "longValue" $ JNIT.methodSignature [] longRet
  longInitMethod <- JNI.getMethodID longClass' "<init>" $ JNIT.methodSignature [longArg] voidRet

  objectClass' <- (JNI.findClass . JNIT.referenceTypeName $ JNIT.SClass "java/lang/Object") >>= mkGlobal
  toStringMethod <- JNI.getMethodID objectClass' "toString" $ JNIT.methodSignature [] strRet
  
  print longValueMethod
  print longInitMethod
  print invokeFns
  print readMethod'
  print varMethod'
  return undefined

newtype Clojure a = Clojure { _runClojure :: ReaderT ClojureCtx IO a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad, MonadIO, MonadReader ClojureCtx)

runClojure_ :: Clojure a -> IO a
runClojure_ m = runInBoundThread . JNI.withJVM clojureOpts $ do
  ctx <- initClojureCtx_
  runReaderT (_runClojure m) ctx

mkGlobal :: Coercible o (JNIT.J ty) => o -> IO o
mkGlobal x = do
  g <- JNI.newGlobalRef x
  JNI.deleteLocalRef x
  return g

main :: IO ()
main = runInBoundThread . JNI.withJVM clojureOpts $ do
  longClass <- (JNI.findClass . JNIT.referenceTypeName $ JNIT.SClass "java/lang/Long") >>= mkGlobal
  let longInitSig = JNIT.methodSignature [SomeSing (sing :: Sing ('JNIT.Prim "long"))] JNIT.SVoid
  longValueM <- JNI.getMethodID longClass "longValue" $ JNIT.methodSignature [] (JNIT.SPrim "long")
  l <- JNI.newObject longClass longInitSig [JNIT.JLong 88]
  i <- JNI.callLongMethod l longValueM []
  print i
  

invoke :: JObject -> [JObject] -> Clojure JObject
invoke fn args = do
  invokeMethods' <- invokeMethods <$> ask
  liftIO $ JNI.callObjectMethod fn ((V.!) invokeMethods' (length args)) (JNIT.JObject <$> args)

varQual :: String -> String -> Clojure JObject
varQual ns fn = do
  varQualMethod' <- varQualMethod <$> ask
  withCString ns
    (\nsCStr ->
       withCString fn
       (\fnCStr -> do
           JNI.callObjectMethod fn varQualMethod' (JNIT.JObject <$> args)
           varObjQualified nsCStr fnCStr >>= mkObject
       )
    )

