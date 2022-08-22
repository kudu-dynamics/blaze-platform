{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Clojure.Core where

import Ghidra.Prelude hiding (toList, first)

import Control.Concurrent
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Foreign (Int64)
import qualified Foreign.JNI as JNI
import qualified Foreign.JNI.Types as JNIT
import Foreign.JNI.Types (JObject)
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)
import qualified Language.Java as Java
import Language.Java (J, VariadicIO)
import qualified Data.ByteString as BS
import System.IO.Memoize (once)
import Control.Concurrent.STM.TMVar
import Prelude.Singletons (Sing, SingI(sing), SomeSing(..))


type JBool = J ('Java.Class "java.lang.Boolean")
type JLong = J ('Java.Class "java.lang.Long")
type JInt = J ('Java.Class "java.lang.Int")
type JString = J ('Java.Class "java.lang.String")

type IFn = J ('Java.Class "clojure.lang.IFn")

clojureJars :: [ByteString]
clojureJars =
  [ "java.class.path=res/clojure/clojure-1.11.1.jar"
  , "res/clojure/spec.alpha-0.3.218.jar"
  ]

mkClojureOpts :: [ByteString] -> [ByteString]
mkClojureOpts nonClojureJars =
  [ "-D" <> BS.intercalate ":" (clojureJars <> nonClojureJars) ]

toString :: Coercible a JObject => a -> IO Text
toString x = Java.call (coerce x :: JObject) "toString" >>= Java.reify

showClass :: JObject -> IO Text
showClass = toString <=< getClass

getClass :: JObject -> IO JObject
getClass x = do
  let fn = unsafeDupablePerformIO $ varQual "clojure.core" "class"
  invoke fn x
  
previousNonClojureJars :: TMVar [ByteString]
previousNonClojureJars = unsafePerformIO newEmptyTMVarIO


-- | You can only call Java.withJVM once in the life of a program, for some reason,
-- according to the tweag jvm lib docs.
-- So we just start it and never stop it. If the opts change, we throw an error.
withJVM :: [ByteString] -> IO a -> IO a
withJVM nonClojureJars action = do
  pjars <- atomically $ isEmptyTMVar previousNonClojureJars >>= \case
    False -> readTMVar previousNonClojureJars
    True -> do
      putTMVar previousNonClojureJars nonClojureJars
      return nonClojureJars
  if pjars == nonClojureJars
    then join . once $ newJVM_
    else error "withJVM: options changed" -- see doc above
  action
  where
    newJVM_ :: IO ()
    newJVM_ = void $ JNI.newJVM (mkClojureOpts nonClojureJars)

runClojure :: [ByteString] -> IO a -> IO a
runClojure nonClojureJars = runInBoundThread . withJVM (mkClojureOpts nonClojureJars)

printObj :: JObject -> IO ()
printObj = Text.putStrLn <=< toString

plus :: [JObject] -> IO JObject
plus xs = do
  let fn = unsafeDupablePerformIO $ varQual "clojure.core" "+"
  applyInvoke fn xs

testClojure :: IO ()
testClojure = do
  let plus = unsafeDupablePerformIO $ varQual "clojure.core" "+"
  x :: JLong <- Java.new (42 :: Int64)
  y :: JLong <- Java.new (88 :: Int64)
  r :: JObject <- Java.call plus "invoke" (coerce x :: JObject) (coerce y :: JObject)
  toString r >>= Text.putStrLn
  
  n <- readEval "(+ 3 8)"
  printObj n
  return ()

varQual :: Text -> Text -> IO IFn
varQual ns fn = do
  ns' <- Java.reflect ns
  fn' <- Java.reflect fn
  Java.callStatic "clojure/java/api/Clojure" "var" (coerce ns' :: JObject) (coerce fn' :: JObject)

readEdn :: Text -> IO JObject
readEdn = Java.callStatic "clojure/java/api/Clojure" "read" <=< Java.reflect

readEval :: Text -> IO JObject
readEval s = do
  let eval = unsafeDupablePerformIO $ varQual "clojure.core" "eval"
  edn <- readEdn s
  invoke eval edn

toClojureString :: Text -> IO JObject
toClojureString t = readEdn $ "\"" <> t <> "\""

nil :: IO JObject
nil = unsafePerformIO . once $ readEval "nil"  

isNil :: JObject -> IO JObject
isNil x = do
  let fn = unsafeDupablePerformIO $ varQual "clojure.core" "nil?"
  Java.call fn "invoke" x

isNil' :: JObject -> IO Bool
isNil' x = isNil x >>= toBool

toBool :: JObject -> IO Bool
toBool x = Java.call (coerce x :: JBool) "booleanValue"

vec :: JObject -> IO JObject
vec ls = do
  let fn = unsafeDupablePerformIO $ varQual "clojure.core" "vec"
  invoke fn ls

invoke
  :: forall a f.
     ( Java.Coercible a
     , VariadicIO f a
     ) => IFn -> f
invoke fn = Java.call fn "invoke"

applyInvoke :: IFn -> [JObject] -> IO JObject
applyInvoke fn args
  = fmap Java.unsafeUncoerce
  $ Java.callToJValue
      (JNIT.SClass "java.lang.Object")
      fn
      "invoke"
      (take (length args) . repeat $ SomeSing (sing :: Sing ('JNIT.Class "java/lang/Object")))
      (JNIT.JObject <$> args)

-- invokeFunc
--   :: forall a f sym.
--      ( Java.Ty a ~ 'Java.Class sym
--      , Coerce.Coercible a (J ('Java.Class sym))
--      , Java.Coercible a
--      , VariadicIO f a
--      ) => Text -> Text -> f
-- invokeFunc ns fn = Java.apply $ \args -> do
--   func <- varQual ns fn
--   Java.unsafeUncoerce <$>
--     Java.callToJValue
--       (sing @(Ty b))
--       (Coerce.coerce obj :: J ty)
--       mname
--       (Java.sings @f Proxy)
--       args

--   Coerce.coerce <$> Java.newJ @sym (Java.sings @f Proxy) args

keyword :: Text -> IO JObject
keyword t = do
  t' :: JObject <- coerce <$> Java.reflect t
  let fn = unsafeDupablePerformIO $ varQual "clojure.core" "keyword"
  invoke fn t'

get :: JObject -> JObject -> IO JObject
get coll key = do
  let fn = unsafeDupablePerformIO $ varQual "clojure.core" "get"
  invoke fn coll key

first :: JObject -> IO JObject
first coll = do
  let fn = unsafeDupablePerformIO $ varQual "clojure.core" "first"
  invoke fn coll

rest :: JObject -> IO JObject
rest coll = do
  let fn = unsafeDupablePerformIO $ varQual "clojure.core" "rest"
  invoke fn coll

toList :: JObject -> IO [JObject]
toList ls = do
  x <- first ls
  b <- isNil' x
  case b of
    True -> return []
    False -> do
      r <- rest ls
      (x:) <$> toList r


apply :: IFn -> [JObject] -> IO JObject
apply fn ls = do
  let apply' = unsafeDupablePerformIO $ varQual "clojure.core" "apply"
  ls' <- Java.reflect ls
  invoke apply' (coerce fn :: JObject) (coerce ls' :: JObject)
  
test :: IO ()
test = runInBoundThread . Java.withJVM [] $ do
  x :: J ('Java.Class "java.lang.Long") <- Java.new (42 :: Int64)
  s :: J ('Java.Class "java.lang.String") <- Java.call x "toString"
  t :: Text <- Java.reify s
  Text.putStrLn $ "Hello " <> t

