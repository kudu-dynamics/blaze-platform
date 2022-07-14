{-# LANGUAGE Haskell2010
    , GADTs
 #-}
{-# OPTIONS -Wall #-}

module Foreign.Java.JNI.Types where

import Prelude

import Data.Int
import Data.Word

import Foreign (ForeignPtr)

data Z = Z deriving Show
data C = C deriving Show
data B = B deriving Show
data S = S deriving Show
data I = I deriving Show
data J = J deriving Show
data F = F deriving Show
data D = D deriving Show
data L = L String deriving Show
data V = V deriving Show
data A x = A x deriving Show
data X = X deriving Show

data Q = Q String deriving Show

data JVM
data JObjectRef
data JClassRef
data JThrowableRef
data JMethodID
data JStaticMethodID
data JFieldID
data JStaticFieldID
data JConstructorID
data JValues
data JChars
data JBytes

data JArg where
    BooleanA :: Bool -> JArg
    CharA    :: Word16 -> JArg
    ByteA    :: Int8 -> JArg
    ShortA   :: Int16 -> JArg
    IntA     :: Int32 -> JArg
    LongA    :: Int64 -> JArg
    FloatA   :: Float -> JArg
    DoubleA  :: Double -> JArg
    StringA  :: String -> JArg
    ObjectA  :: (Maybe JObject) -> JArg
    ArrayA   :: (Maybe (JArray e)) -> JArg

-- | A reference to an arbitrary Object.
newtype JObject = JObject { jobjectPtr :: ForeignPtr JObjectRef }
    deriving Show

-- | A reference to a Class object.
newtype JClass = JClass { jclassPtr :: ForeignPtr JClassRef }
    deriving Show

-- | A reference to an Exception.
newtype JThrowable = JThrowable { jthrowablePtr :: ForeignPtr JThrowableRef }
    deriving Show

-- | A reference to an Array in the JVM.
data JArray e = JArray {
    jarrayLength :: Int32,
    jarrayPtr :: ForeignPtr JObjectRef
  } deriving Show


