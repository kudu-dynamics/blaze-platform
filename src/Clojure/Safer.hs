{-# LANGUAGE DataKinds #-}
module Clojure.Safer where

import Prelude
-- import qualified Clojure.JNI as JNI
import Control.Concurrent
import Control.Exception (bracket, catch)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import qualified Data.Text.IO as Text
import Data.Vector (Vector)
import qualified Data.Vector as V
import Foreign
import Foreign.C.Types
import qualified Foreign.JNI as JNI
import qualified Foreign.JNI.Types as JNIT
import Foreign.JNI.Types (JClass, JMethodID, JObject, MethodSignature)
import Foreign.C.String (withCString)
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

  invokeMethods' <- V.forM (V.fromListN 20 [0..]) $ \n -> do
    let invokeSig = JNIT.methodSignature (replicate n objArg) objRet
    JNI.getMethodID ifnClass' "invoke" invokeSig

  longClass' <- (JNI.findClass . JNIT.referenceTypeName $ JNIT.SClass "java/lang/Long") >>= mkGlobal
  longValueMethod' <- JNI.getMethodID longClass' "longValue" $ JNIT.methodSignature [] longRet
  longInitMethod' <- JNI.getMethodID longClass' "<init>" $ JNIT.methodSignature [longArg] voidRet

  objectClass' <- (JNI.findClass . JNIT.referenceTypeName $ JNIT.SClass "java/lang/Object") >>= mkGlobal
  toStringMethod' <- JNI.getMethodID objectClass' "toString" $ JNIT.methodSignature [] strRet

  return $ ClojureCtx
    { clojureClass = clojureClass'
    , readMethod = readMethod'
    , varMethod = varMethod'
    , varQualMethod = varQualMethod'
    , ifnClass = ifnClass'
    , invokeMethods = invokeMethods'

    , longClass = longClass'
    , longValueMethod = longValueMethod'
    , longInitMethod = longInitMethod'

    , objectClass = objectClass'
    , toStringMethod = toStringMethod'
    }

newtype Clojure a = Clojure { _runClojure :: ReaderT ClojureCtx IO a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad, MonadIO, MonadReader ClojureCtx)

runClojure_ :: Clojure a -> IO a
runClojure_ m = runInBoundThread . JNI.withJVM clojureOpts . JNI.runInAttachedThread $ do
  ctx <- initClojureCtx_
  runReaderT (_runClojure m) ctx

mkGlobal :: Coercible o (JNIT.J ty) => o -> IO o
mkGlobal x = JNI.runInAttachedThread $ do
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

test :: IO ()
test = runClojure_ $ do
  l <- long 88
  t <- toText l
  liftIO $ Text.putStrLn t
  nums <- traverse long [100, 5, 75]
  liftIO $ Text.putStrLn "ok1"
  sum' <- plus nums
  liftIO $ Text.putStrLn "ok2"
  t' <- toText sum'
  liftIO $ Text.putStrLn "ok3"
  liftIO $ Text.putStrLn t'

tryTest :: IO ()
tryTest = catch test (Text.putStrLn <=< JNI.showException)

invoke :: JObject -> [JObject] -> Clojure JObject
invoke fn args
  | length args > 20 = error "Cannot currently handle more than 20 args"
  | otherwise = do
      invokeMethods' <- invokeMethods <$> ask
      liftIO . JNI.runInAttachedThread $ JNI.callObjectMethod fn ((V.!) invokeMethods' (length args)) (JNIT.JObject <$> args)
        >>= mkGlobal

varQual :: Text -> Text -> Clojure JObject
varQual ns fn = do
  ctx <- ask
  liftIO . JNI.runInAttachedThread $ do
    putStrLn "vq1"
    ns' <- Text.useAsPtr ns (\t -> JNI.newString t . fromIntegral)
    putStrLn "vq2"
    fn' <- Text.useAsPtr fn (\t -> JNI.newString t . fromIntegral)
    putStrLn "vq3"
    r <- JNI.runInAttachedThread $ JNI.callStaticObjectMethod
           (clojureClass ctx)
           (varQualMethod ctx)
           [JNIT.JObject ns', JNIT.JObject fn']
    putStrLn "vq4"
    mkGlobal r

toText :: JObject -> Clojure Text
toText x = do
  toStringMethod' <- toStringMethod <$> ask
  liftIO . JNI.runInAttachedThread $ do
    js <- coerce <$> JNI.callObjectMethod x toStringMethod' []
    len <- JNI.getStringLength js
    charsPtr <- JNI.getStringChars js
    t <- Text.fromPtr charsPtr (fromIntegral len)
    JNI.releaseStringChars js charsPtr
    return t
    
long :: Int64 -> Clojure JObject
long n = do
  ctx <- ask
  liftIO . JNI.runInAttachedThread $ JNI.newObject_ (longClass ctx) (longInitMethod ctx) [JNIT.JLong n] >>= mkGlobal


-------------------------

--- These functions don't call any JNI stuff directly

plusFn :: Clojure JObject
plusFn = varQual "clojure.core" "+"

plus :: [JObject] -> Clojure JObject
plus nums = do
  fn <- varQual "clojure.core" "+"
  t <- toText fn
  liftIO $ Text.putStrLn t
  invoke fn nums
