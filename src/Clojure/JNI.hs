{-# LANGUAGE ForeignFunctionInterface #-}

module Clojure.JNI where


import Prelude
import Foreign
import Foreign.C.Types
import Foreign.C.String

-- Clojure objects are just Java objects, and jsvalue is a union with size 64
-- bits. Since we are cutting corners, we might as well just derive 'Storable'
-- from something else that has the same size - 'CLong'.
newtype ClojureObject = ClojureObject CLong deriving newtype (Storable)

foreign import ccall "load_methods" load_methods :: IO ()
foreign import ccall "create_vm" create_vm :: IO ()
foreign import ccall "check_exception" checkException :: IO ()
foreign import ccall "invokeFn" invokeFn :: ClojureObject -> CUInt -> Ptr ClojureObject -> IO ClojureObject
foreign import ccall "readObj" readObj :: CString -> IO ClojureObject
foreign import ccall "varObj" varObj :: CString -> IO ClojureObject
foreign import ccall "varObjQualified" varObjQualified :: CString -> CString -> IO ClojureObject
foreign import ccall "newLong" newLong :: CLong -> ClojureObject
foreign import ccall "longValue" longValue :: ClojureObject -> CLong

-- | In order for anything to work, this needs to be called first.
loadClojure :: IO ()
loadClojure = create_vm *> load_methods

-- | Make a Clojure function call
invoke :: ClojureObject -> [ClojureObject] -> IO ClojureObject
invoke fn args = do
  args' <- newArray args
  let n = fromIntegral (length args)
  invokeFn fn n args'

-- | Make a Clojure number from a Haskell one
long :: Int64 -> ClojureObject
long l = newLong (CLong l)

-- | Make a Haskell number from a Clojure one
unLong :: ClojureObject -> Int64
unLong cl = let CLong l = longValue cl in l

readEdn :: String -> IO ClojureObject
readEdn s = withCString s readObj

-- -- | Look up a var in Clojure based on the namespace and name
-- varQual :: String -> String -> IO ClojureObject
-- varQual ns fn = withCString ns (\nsCStr ->
--                 withCString fn (\fnCStr -> varObjQualified nsCStr fnCStr))

-- | Look up a var in Clojure based on the namespace and name
varQual :: String -> String -> IO ClojureObject
varQual ns fn = withCString (ns <> "/" <> fn) varObj

method0 :: String -> ClojureObject -> IO ClojureObject
method0 methodName x = do
  eval <- varQual "clojure.core" "eval"
  checkException
  putStrLn "yes 1"
  methSym <- readEdn $ "(fn [x] (." <> methodName <> " x))"
  checkException
  putStrLn "yes 2"
  meth <- invoke eval [methSym]
  checkException
  putStrLn "yes 3"
  invoke meth [x]

main :: IO ()
main = do
  loadClojure
  putStrLn "Clojure loaded"
  eval <- varQual "clojure.core" "eval"
  plus <- varQual "clojure.core" "+"
  putStrLn "really works"
  return ()
  out <- invoke plus [long 3, long 4]
  print $ unLong out -- prints "7" on my tests
  out2 <- invoke plus [long 100, out]
  print $ unLong out2

  plusSymbol <- readEdn "+"
  plus' <- invoke eval [plusSymbol]
  out3 <- invoke plus' [long 8, long 10]
  print $ unLong out3

  out3' <- method0 "byteValue" out3
  checkException
  print $ unLong out3'
  print $ unLong out3
