module Clojure.Unsafe where

import Prelude
import qualified Clojure.JNI as JNI
import Foreign
import Foreign.C.Types
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)


-- newtype ClojureObject = ClojureObject { _unClojureObject :: (ForeignPtr JNI.ClojureObject) }

-- mkObject :: Ptr JNI.ClojureObject -> IO ClojureObject
-- mkObject = fmap ClojureObject . newForeignPtr JNI.deleteGlobalRef


-- -- -- This might be really unsafe. The `withForeignPtr` call finishes before the pointers are actually used, so maybe they could get GC'd and all would crash?
-- -- -- TODO: check how c2hs does it
-- -- withForeignPtrs :: forall a b.[ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
-- -- withForeignPtrs fptrs f = getPtrs >>= f
-- --   where
-- --     getPtrs :: IO [Ptr a]
-- --     getPtrs = traverse (flip withForeignPtr return) fptrs

-- -- I'm not really sure this works.
-- -- The idea is to keep each withForeignPtr unfinished until f is executed
-- withForeignPtrs :: forall a b. [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
-- withForeignPtrs [] f = f []
-- withForeignPtrs (x:xs) f = withForeignPtr x $ withForeignPtrs xs . mkNewF
--   where
--     mkNewF :: Ptr a -> [Ptr a] -> IO b
--     mkNewF ptr ptrs = f (ptr:ptrs)

-- withClojureObject :: ClojureObject -> (Ptr JNI.ClojureObject -> IO b) -> IO b
-- withClojureObject (ClojureObject fptr) = withForeignPtr fptr

-- withClojureObjects :: [ClojureObject] -> ([Ptr JNI.ClojureObject] -> IO b) -> IO b
-- withClojureObjects xs = withForeignPtrs (_unClojureObject <$> xs)

-- invoke :: ClojureObject -> [ClojureObject] -> IO ClojureObject
-- invoke fn args = withClojureObject fn $ \fnPtr -> do
--   withClojureObjects args $ \argPtrs -> do
--     JNI.invoke fnPtr argPtrs >>= mkObject

-- long :: Int64 -> ClojureObject
-- long n = unsafePerformIO $ do
--   putStrLn "hello"
--   mkObject . JNI.long $ n

-- unLong :: ClojureObject -> IO 
