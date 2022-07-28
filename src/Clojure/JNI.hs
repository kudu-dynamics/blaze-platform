{-# LANGUAGE ForeignFunctionInterface #-}

module Clojure.JNI where


import Prelude
import Control.Exception (throwIO, Exception)
import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Control.Concurrent

-- Clojure objects are just Java objects, and jsvalue is a union with size 64
-- bits. Since we are cutting corners, we might as well just derive 'Storable'
-- from something else that has the same size - 'CLong'.
-- data ClojureObject
newtype ClojureObject = ClojureObject { _unClojureObject :: (ForeignPtr ClojureObject) }

type JString = ClojureObject

data JNIEnv

foreign import ccall "load_methods" load_methods :: IO ()
foreign import ccall "create_vm" create_vm :: IO CBool
foreign import ccall "check_exception" checkException_ :: IO CBool
foreign import ccall "print_exception" printException :: IO ()

foreign import ccall "getThreadEnv" getThreadEnv :: IO (Ptr JNIEnv)
foreign import ccall "invokeFnE" invokeFnE :: Ptr JNIEnv -> (Ptr ClojureObject) -> CUInt -> Ptr (Ptr ClojureObject) -> IO (Ptr ClojureObject)
foreign import ccall "readObjE" readObjE :: Ptr JNIEnv -> CString -> IO (Ptr ClojureObject)
foreign import ccall "varObjE" varObjE :: Ptr JNIEnv -> CString -> IO (Ptr ClojureObject)
foreign import ccall "varObjQualifiedE" varObjQualifiedE :: Ptr JNIEnv -> CString -> CString -> IO (Ptr ClojureObject)
foreign import ccall "newLongE" newLongE :: Ptr JNIEnv -> CLong -> IO (Ptr ClojureObject)
foreign import ccall "longValueE" longValueE :: Ptr JNIEnv -> (Ptr ClojureObject) -> IO CLong
foreign import ccall "newBooleanE" newBooleanE :: Ptr JNIEnv -> CBool -> IO (Ptr ClojureObject)
foreign import ccall "booleanValueE" booleanValueE :: Ptr JNIEnv -> (Ptr ClojureObject) -> IO CBool
foreign import ccall "getStringUTFCharsE" getStringUTFCharsE :: Ptr JNIEnv -> Ptr JString -> IO CString
foreign import ccall "releaseStringUTFCharsE" releaseStringUTFCharsE :: Ptr JNIEnv -> Ptr JString -> CString -> IO ()
foreign import ccall "toStringE" toStringE :: Ptr JNIEnv -> Ptr ClojureObject -> IO (Ptr JString)
foreign import ccall "deleteGlobalRefE" deleteGlobalRefE :: Ptr JNIEnv -> (Ptr ClojureObject) -> IO ()


-- The below functions don't need an Env passed in and can be run in any native thread
-- because each one checks which thread they are in and whether it already has a classloader,
-- and loads one otherwise.  This adds some overhead, however.
foreign import ccall "invokeFn" invokeFn :: (Ptr ClojureObject) -> CUInt -> Ptr (Ptr ClojureObject) -> IO (Ptr ClojureObject)
foreign import ccall "readObj" readObj :: CString -> IO (Ptr ClojureObject)
foreign import ccall "varObj" varObj :: CString -> IO (Ptr ClojureObject)
foreign import ccall "varObjQualified" varObjQualified :: CString -> CString -> IO (Ptr ClojureObject)
foreign import ccall "newLong" newLong :: CLong -> IO (Ptr ClojureObject)
foreign import ccall "longValue" longValue :: Ptr ClojureObject -> IO CLong
foreign import ccall "newBoolean" newBoolean :: CBool -> IO (Ptr ClojureObject)
foreign import ccall "booleanValue" booleanValue :: Ptr ClojureObject -> IO CBool

foreign import ccall "getStringUTFChars" getStringUTFChars :: (Ptr JString) -> IO CString
foreign import ccall "releaseStringUTFChars" releaseStringUTFChars :: Ptr JString -> CString -> IO ()
foreign import ccall "toString" toString_ :: Ptr ClojureObject -> IO (Ptr JString)
foreign import ccall "deleteGlobalRef" deleteGlobalRef :: FunPtr ((Ptr a) -> IO ())

data JavaException = JavaException deriving (Show, Exception)

mkObject :: Ptr ClojureObject -> IO ClojureObject
mkObject x = do
  -- fmap ClojureObject . newForeignPtr deleteGlobalRef $ x
  fmap ClojureObject . newForeignPtr_ $ x -- deleteGlobalRef $ x

-- I'm not really sure this works.
-- The idea is to keep each withForeignPtr unfinished until f is executed
withForeignPtrs :: forall a b. [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs [] f = f []
withForeignPtrs (x:xs) f = withForeignPtr x $ withForeignPtrs xs . mkNewF
  where
    mkNewF :: Ptr a -> [Ptr a] -> IO b
    mkNewF ptr ptrs = f (ptr:ptrs)

withClojureObject :: ClojureObject -> (Ptr ClojureObject -> IO b) -> IO b
withClojureObject (ClojureObject fptr) = withForeignPtr fptr

withClojureObjects :: [ClojureObject] -> ([Ptr ClojureObject] -> IO b) -> IO b
withClojureObjects xs = withForeignPtrs (_unClojureObject <$> xs)


-- | Checks for java exception and throws haskell error
checkException :: IO ()
checkException = toBool <$> checkException_ >>= \case
  False -> return ()
  True -> do
    printException
    throwIO JavaException

-- | In order for anything to work, this needs to be called first.
loadClojure :: IO ()
loadClojure = toBool <$> create_vm >>= \case
  True -> load_methods
  False -> return ()

-- | Make a Clojure function call
invoke_ :: (Ptr ClojureObject) -> [(Ptr ClojureObject)] -> IO (Ptr ClojureObject)
invoke_ fn args = do
  args' <- newArray args
  let n = fromIntegral (length args)
  invokeFn fn n args'

invoke :: ClojureObject -> [ClojureObject] -> IO ClojureObject
invoke fn args = withClojureObject fn $ \fnPtr -> do
  withClojureObjects args $ \argPtrs -> do
    invoke_ fnPtr argPtrs >>= mkObject

-- | Make a Clojure number from a Haskell one
long :: Int64 -> IO ClojureObject
long l = do
  x <- newLong (CLong l)
  -- putStrLn "Got it ok"
  checkException
  mkObject x

-- | Make a Haskell number from a Clojure one
unLong :: ClojureObject -> IO Int64
unLong cl = withClojureObject cl $ \ptr -> do
  (CLong l) <- longValue ptr
  return l

unString :: Ptr JString -> IO String
unString jstr = do
  cstr <- getStringUTFChars jstr
  str <- peekCString cstr
  -- releaseStringUTFChars jstr cstr
  return str

readEdn :: String -> IO ClojureObject
readEdn s = withCString s $ mkObject <=< readObj

-- showClassLoader :: IO ()
-- showClassLoader = do
--   eval <- varQual "clojure.core" "eval"
--   c <- readEdn "(.getContextClassLoader (java.lang.Thread/currentThread))"
--   x <- invoke eval [c]
--   print x

-- | Look up a var in Clojure based on the namespace and name
varQual2 :: String -> String -> IO ClojureObject
varQual2 ns fn = withCString ns
  (\nsCStr ->
      withCString fn
      (\fnCStr ->
         varObjQualified nsCStr fnCStr >>= mkObject))

-- | Look up a var in Clojure based on the namespace and name
varQual :: String -> String -> IO ClojureObject
varQual ns fn = withCString (ns <> "/" <> fn) (mkObject <=< varObj)

readEval :: String -> IO ClojureObject
readEval s = do
  -- putStrLn $ "readEval: " <> s
  eval <- varQual "clojure.core" "eval"
  edn <- readEdn s
  invoke eval [edn]

method0 :: String -> ClojureObject -> IO ClojureObject
method0 methodName x = do
  eval <- varQual "clojure.core" "eval"
  checkException
  methSym <- readEdn $ "(fn [x] (." <> methodName <> " x))"
  checkException
  meth <- invoke eval [methSym]
  checkException
  r <- invoke meth [x]
  return r

-- instance Show ClojureObject where
--   show x = unsafePerformIO $ do
--     s <- method0 "toString" x
--     checkException
--     unString s


main2 :: IO ()
main2 = do
  loadClojure
  -- showClassLoader
  putStrLn "Clojure loaded"
  plus <- varQual2 "clojure.core" "+"
  -- print plus
  return ()

toString :: ClojureObject -> IO String
toString obj = withClojureObject obj $ \pobj -> do
  jstr <- toString_ pobj
  cstr <- getStringUTFChars jstr
  str <- peekCString cstr
  releaseStringUTFChars jstr cstr
  return str

printObj :: ClojureObject -> IO ()
printObj = putStrLn <=< toString


test :: IO ()
test = runInBoundThread $ do
  n <- readEval "(+ 34 88)"
  printObj n

  _ <- readEval "(require (quote [ghidra-clojure.state]))"
  _ <- readEval "(require (quote [ghidra-clojure.function]))"

  gs <- openDatabase "/tmp/kudu/assembly/contrived4" >>= analyze

  funcs <- getFunctions gs >>= vec >>= toList

  mapM_ printObj funcs

--------- Clojure Functions --------------

toClojureString :: String -> IO ClojureObject
toClojureString t = readEdn $ "\"" <> t <> "\""

vec :: ClojureObject -> IO ClojureObject
vec ls = do
  fn <- varQual "clojure.core" "vec"
  invoke fn [ls]

invokeFunc :: String -> String -> [ClojureObject] -> IO ClojureObject
invokeFunc ns funcName args = do
  fn <- varQual ns funcName
  invoke fn args
  
isNil :: ClojureObject -> IO ClojureObject
isNil x = invokeFunc "clojure.core" "nil?" [x]

isNil' :: ClojureObject -> IO Bool
isNil' x = do
  jb <- isNil x
  toBool <$> withClojureObject jb booleanValue

first :: ClojureObject -> IO ClojureObject
first coll = invokeFunc "clojure.core" "first" [coll]

rest :: ClojureObject -> IO ClojureObject
rest coll = invokeFunc "clojure.core" "rest" [coll]

toList :: ClojureObject -> IO [ClojureObject]
toList ls = do
  x <- first ls
  checkException
  b <- isNil' x
  checkException
  case b of
    True -> return []
    False -> do
      r <- rest ls
      (x:) <$> toList r
  
  
-------- Ghidra Functions ---------------

openDatabase :: FilePath -> IO ClojureObject
openDatabase fp = do
  openDb <- varQual "ghidra-clojure.state" "open-database"
  fp' <- toClojureString fp
  putStrLn "open db"
  printObj fp'
  invoke openDb [fp']

analyze :: ClojureObject -> IO ClojureObject
analyze db = do
  fn <- varQual "ghidra-clojure.state" "analyze"
  invoke fn [db]
  
getFunctions :: ClojureObject -> IO ClojureObject
getFunctions gs = do
  fn <- varQual "ghidra-clojure.function" "get-functions"
  checkException
  invoke fn [gs]
