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
foreign import ccall "getStringUTFChars" getStringUTFChars :: (Ptr JString) -> IO CString
foreign import ccall "releaseStringUTFChars" releaseStringUTFChars :: Ptr JString -> CString -> IO ()
foreign import ccall "toString" toString :: Ptr ClojureObject -> IO (Ptr JString)
foreign import ccall "deleteGlobalRef" deleteGlobalRef :: FunPtr ((Ptr a) -> IO ())

data JavaException = JavaException deriving (Show, Exception)

mkObject :: Ptr ClojureObject -> IO ClojureObject
mkObject x = do
  putStrLn $ "mkObject: " <> show x
  fmap ClojureObject . newForeignPtr deleteGlobalRef $ x

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
  putStrLn "Got it ok"
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

-- main :: IO ()
-- main = do
--   loadClojure
--   putStrLn "Clojure loaded"
--   eval <- varQual "clojure.core" "eval"
--   plus <- varQual "clojure.core" "+"
--   minus <- varQual "clojure.core" "-"
--   putStrLn "really works"
--   return ()
--   out <- invoke plus [long 3, long 4]
--   print $ unLong out -- prints "7" on my tests
--   out2 <- invoke plus [long 100, out]
--   print $ unLong out2

--   plusSymbol <- readEdn "+"
--   plus' <- invoke eval [plusSymbol]
--   out3 <- invoke plus' [long 8, long 10]
--   print $ unLong out3

--   out3' <- method0 "byteValue" out3
--   checkException
--   print $ unLong out3'
--   print $ unLong out3
--   -- print out3


jackson :: Int
jackson = unsafePerformIO $ do
  putStrLn "hey guys"
  return 5


jackson2 :: Int -> Int
jackson2 n = unsafePerformIO $ do
  putStrLn "hey"
  return 5


test :: IO ()
test = runInBoundThread $ do
  putStrLn "HEY 0"
  x <- long 34
  putStrLn "hey 1"
  -- jstr <- withClojureObject x toString
  -- putStrLn "hey 2"
  -- s <- unString jstr
  -- putStrLn "hey 3"
  -- putStrLn s
  putStrLn "got it"
