{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS -Wall #-}

-- |
-- Module       : Foreign.Java.JNI.%NAME%
-- Copyright    : (c) Julian Fleischer 2013
-- License      : MIT (See LICENSE file in cabal package)
--
-- Maintainer   : julian.fleischer@fu-berlin.de
-- Stability    : stable
-- Portability  : portable (Haskell2010)
--
-- Low level interface to the Java Virtual Machine.
-- This module is a very thin wrapper over the Java Native Interface.
--
-- Note that all functions that return references (of type @Ptr JObjectRef@)
-- return global references which you will need to free manually.
-- 
-- This module contains %SAFETY% bindings, see also
-- "Foreign.Java.JNI.%OPPOSITE%".
-- 
-- Read the /The Java Native Interface - Programmer's Guide and Specification/
-- for further information on the Java Native Interface
-- (available at <http://www.soi.city.ac.uk/~kloukin/IN2P3/material/jni.pdf>,
--  and <http://192.9.162.55/docs/books/jni/>).
module Foreign.Java.JNI.Core (

-- * Controlling the virtual machine

JVM,

createVM,
createVM',
destroyVM,
persistVM,

-- * Discovering classes

JClassRef,
findClass,

-- * Object creation

JConstructorID,
JObjectRef,

getConstructorID,
newObject,

-- ** Array creation

-- newBooleanArray,
-- newCharArray,
-- newLongArray,
-- newIntArray,
-- newShortArray,
-- newByteArray,
-- newFloatArray,
-- newDoubleArray,
-- newObjectArray,

-- * Method access

JStaticMethodID,
JMethodID,

getStaticMethodID,
getMethodID,

-- ** Static method invocation

callStaticVoidMethod,
callStaticBooleanMethod,
callStaticCharMethod,
callStaticByteMethod,
callStaticShortMethod,
callStaticIntMethod,
callStaticLongMethod,
callStaticFloatMethod,
callStaticDoubleMethod,
callStaticObjectMethod,
callStaticStringMethod,

-- ** Method invocation

callVoidMethod,
callBooleanMethod,
callCharMethod,
callByteMethod,
callShortMethod,
callIntMethod,
callLongMethod,
callFloatMethod,
callDoubleMethod,
callObjectMethod,
callStringMethod,

-- * Field access

JFieldID,
JStaticFieldID,

getFieldID,
getStaticFieldID,

-- ** Static getters

getStaticBooleanField,
getStaticCharField,
getStaticByteField,
getStaticShortField,
getStaticIntField,
getStaticLongField,
getStaticFloatField,
getStaticDoubleField,
getStaticObjectField,
getStaticStringField,

-- ** Static setters

setStaticBooleanField,
setStaticCharField,
setStaticByteField,
setStaticShortField,
setStaticIntField,
setStaticLongField,
setStaticFloatField,
setStaticDoubleField,
setStaticObjectField,
setStaticStringField,

-- ** Member getters

getBooleanField,
getCharField,
getByteField,
getShortField,
getIntField,
getLongField,
getFloatField,
getDoubleField,
getObjectField,
getStringField,

-- ** Member setters

setBooleanField,
setCharField,
setByteField,
setShortField,
setIntField,
setLongField,
setFloatField,
setDoubleField,
setObjectField,
setStringField,

-- * Argument passing

JValues,
JArg (..),

mkJValues,

newJValues,
setJValueByte,
setJValueShort,
setJValueInt,
setJValueLong,
setJValueFloat,
setJValueDouble,
setJValueObject,
setJValueString,

-- * Releasing resources

releaseJObjectRef,
releaseJClassRef,
releaseJThrowableRef,
release,

-- * Special data types

-- ** String handling

JChars,
JBytes,

newJString,
charsFromJString,
bytesFromJString,
releaseJChars,
releaseJBytes,
jStringToCString,

-- ** Array handling

getArrayLength,

-- ** Reflection

getObjectClass,
isInstanceOf,

-- ** Exception handling

JThrowableRef,

exceptionCheck,
exceptionOccurred,
exceptionOccurredClear,
exceptionClear,
exceptionDescribe,

-- * Debugging

getDebugStatus,
setDebugStatus,

-- * libjvm initialization

getLibjvmPath,
getCompiledLibjvmPath,
setLibjvmPath,
registerCallbacks,

-- * Primitive types

Z (Z), C (C), B (B), S (S), I (I), J (J),
D (D), F (F), L (L), V (V), A (A), X (X),

-- * Workarounds for certain platforms

runCocoaMain

) where

import Prelude
import Data.Int
import Data.Word

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Foreign.Java.JNI.Types

-- | Create a JValues Array which can be used for argument
-- passing to a multi parameter method. You need to free the
-- Ptr object manually using 'free'.
--
-- This method is implemented using @calloc@ internally.
-- See the native implementation of @newJValues@.
mkJValues :: Ptr JVM -> [JArg] -> IO (Ptr JValues)
mkJValues vm args = do
    jvalues <- newJValues $ fromIntegral $ length args
    fillJValuesArray vm (0 :: CInt) args jvalues

fillJValuesArray :: Ptr JVM -> CInt -> [JArg] -> Ptr JValues -> IO (Ptr JValues)
fillJValuesArray _ _ [] jvalues = return jvalues
fillJValuesArray vm ix (x:xs) jvalues = setValue >> fillJValuesArray vm (ix+1) xs jvalues
    where
        setValue =
            case x of
                (BooleanA v) -> setJValueBoolean jvalues ix v
                (CharA v)    -> setJValueChar    jvalues ix v
                (ByteA v)    -> setJValueByte    jvalues ix v
                (ShortA v)   -> setJValueShort   jvalues ix v
                (IntA v)     -> setJValueInt     jvalues ix v
                (LongA v)    -> setJValueLong    jvalues ix v
                (FloatA v)   -> setJValueFloat   jvalues ix $ realToFrac v
                (DoubleA v)  -> setJValueDouble  jvalues ix $ realToFrac v
                (ObjectA (Just v)) -> withForeignPtr (jobjectPtr v) $
                                            \ptr -> setJValueObject jvalues ix ptr
                (ObjectA Nothing) -> setJValueObject jvalues ix nullPtr
                (ArrayA (Just v)) -> withForeignPtr (jarrayPtr v) $
                                            \ptr -> setJValueObject jvalues ix ptr
                (ArrayA Nothing) -> setJValueObject jvalues ix nullPtr
                (StringA v)  -> do
                    cstring <- newCString v
                    setJValueString vm jvalues ix cstring
                    free cstring

foreign import ccall safe "ffijni.h createVM"
    createVM :: IO (Ptr JVM)

foreign import ccall safe "ffijni.h createVM2"
    createVM' :: Word32 -- ^ The number of arguments (like argc)
              -> Ptr CString -- ^ A @char**@ to the arguments (like argv)
              -> IO (Ptr JVM) -- ^ Returns a Ptr to the newly running JVM

foreign import ccall safe "ffijni.h destroyVM"
    destroyVM :: Ptr JVM -> IO ()

foreign import ccall safe "ffijni.h persistVM"
    persistVM :: Ptr JVM -> IO ()

foreign import ccall safe "ffijni.h findClass"
    findClass :: Ptr JVM -> CString -> IO (Ptr JClassRef)

foreign import ccall safe "ffijni.h newObject"
    newObject :: Ptr JVM -> Ptr JClassRef -> Ptr JConstructorID -> Ptr JValues -> IO (Ptr JObjectRef)

foreign import ccall safe "ffijni.h getConstructorID"
    getConstructorID :: Ptr JVM -> Ptr JClassRef -> CString -> IO (Ptr JConstructorID)

foreign import ccall safe "ffijni.h getStaticMethodID"
    getStaticMethodID :: Ptr JVM -> Ptr JClassRef -> CString -> CString -> IO (Ptr JStaticMethodID)

foreign import ccall safe "ffijni.h getMethodID"
    getMethodID :: Ptr JVM -> Ptr JClassRef -> CString -> CString -> IO (Ptr JMethodID)

foreign import ccall safe "ffijni.h getStaticFieldID"
    getStaticFieldID :: Ptr JVM -> Ptr JClassRef -> CString -> CString -> IO (Ptr JStaticFieldID)

foreign import ccall safe "ffijni.h getFieldID"
    getFieldID :: Ptr JVM -> Ptr JClassRef -> CString -> CString -> IO (Ptr JFieldID)

foreign import ccall safe "ffijni.h callStaticVoidMethod"
    callStaticVoidMethod :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticMethodID -> Ptr JValues -> IO ()

foreign import ccall safe "ffijni.h callStaticIntMethod"
    callStaticIntMethod :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticMethodID -> Ptr JValues -> IO Int32

foreign import ccall safe "ffijni.h callStaticLongMethod"
    callStaticLongMethod :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticMethodID -> Ptr JValues -> IO Int64

foreign import ccall safe "ffijni.h callStaticShortMethod"
    callStaticShortMethod :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticMethodID -> Ptr JValues -> IO Int16

foreign import ccall safe "ffijni.h callStaticByteMethod"
    callStaticByteMethod :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticMethodID -> Ptr JValues -> IO Int8

foreign import ccall safe "ffijni.h callStaticFloatMethod"
    callStaticFloatMethod :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticMethodID -> Ptr JValues -> IO CFloat

foreign import ccall safe "ffijni.h callStaticDoubleMethod"
    callStaticDoubleMethod :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticMethodID -> Ptr JValues -> IO CDouble

foreign import ccall safe "ffijni.h callStaticBooleanMethod"
    callStaticBooleanMethod :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticMethodID -> Ptr JValues -> IO Bool

foreign import ccall safe "ffijni.h callStaticCharMethod"
    callStaticCharMethod :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticMethodID -> Ptr JValues -> IO Word16

foreign import ccall safe "ffijni.h callStaticObjectMethod"
    callStaticObjectMethod :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticMethodID -> Ptr JValues -> IO (Ptr JObjectRef)

foreign import ccall safe "ffijni.h callStaticStringMethod"
    callStaticStringMethod :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticMethodID -> Ptr JValues -> IO CString

foreign import ccall safe "ffijni.h callVoidMethod"
    callVoidMethod :: Ptr JVM -> Ptr JObjectRef -> Ptr JMethodID -> Ptr JValues -> IO ()

foreign import ccall safe "ffijni.h callLongMethod"
    callLongMethod :: Ptr JVM -> Ptr JObjectRef -> Ptr JMethodID -> Ptr JValues -> IO Int64

foreign import ccall safe "ffijni.h callIntMethod"
    callIntMethod :: Ptr JVM -> Ptr JObjectRef -> Ptr JMethodID -> Ptr JValues -> IO Int32

foreign import ccall safe "ffijni.h callShortMethod"
    callShortMethod :: Ptr JVM -> Ptr JObjectRef -> Ptr JMethodID -> Ptr JValues -> IO Int16

foreign import ccall safe "ffijni.h callByteMethod"
    callByteMethod :: Ptr JVM -> Ptr JObjectRef -> Ptr JMethodID -> Ptr JValues -> IO Int8

foreign import ccall safe "ffijni.h callFloatMethod"
    callFloatMethod :: Ptr JVM -> Ptr JObjectRef -> Ptr JMethodID -> Ptr JValues -> IO CFloat

foreign import ccall safe "ffijni.h callDoubleMethod"
    callDoubleMethod :: Ptr JVM -> Ptr JObjectRef -> Ptr JMethodID -> Ptr JValues -> IO CDouble

foreign import ccall safe "ffijni.h callBooleanMethod"
    callBooleanMethod :: Ptr JVM -> Ptr JObjectRef -> Ptr JMethodID -> Ptr JValues -> IO Bool

foreign import ccall safe "ffijni.h callCharMethod"
    callCharMethod :: Ptr JVM -> Ptr JObjectRef -> Ptr JMethodID -> Ptr JValues -> IO Word16

foreign import ccall safe "ffijni.h callObjectMethod"
    callObjectMethod :: Ptr JVM -> Ptr JObjectRef -> Ptr JMethodID -> Ptr JValues -> IO (Ptr JObjectRef)

foreign import ccall safe "ffijni.h callStringMethod"
    callStringMethod :: Ptr JVM -> Ptr JObjectRef -> Ptr JMethodID -> Ptr JValues -> IO CString

foreign import ccall safe "ffijni.h getStaticLongField"
    getStaticLongField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> IO Int64

foreign import ccall safe "ffijni.h getStaticIntField"
    getStaticIntField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> IO Int32

foreign import ccall safe "ffijni.h getStaticShortField"
    getStaticShortField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> IO Int16

foreign import ccall safe "ffijni.h getStaticByteField"
    getStaticByteField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> IO Int8

foreign import ccall safe "ffijni.h getStaticFloatField"
    getStaticFloatField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> IO CFloat

foreign import ccall safe "ffijni.h getStaticDoubleField"
    getStaticDoubleField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> IO CDouble

foreign import ccall safe "ffijni.h getStaticBooleanField"
    getStaticBooleanField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> IO Bool

foreign import ccall safe "ffijni.h getStaticCharField"
    getStaticCharField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> IO Word16

foreign import ccall safe "ffijni.h getStaticObjectField"
    getStaticObjectField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> IO (Ptr JObjectRef)

foreign import ccall safe "ffijni.h getStaticStringField"
    getStaticStringField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> IO CString

foreign import ccall safe "ffijni.h getLongField"
    getLongField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> IO Int64

foreign import ccall safe "ffijni.h getIntField"
    getIntField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> IO Int32

foreign import ccall safe "ffijni.h getShortField"
    getShortField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> IO Int16

foreign import ccall safe "ffijni.h getByteField"
    getByteField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> IO Int8

foreign import ccall safe "ffijni.h getFloatField"
    getFloatField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> IO CFloat

foreign import ccall safe "ffijni.h getDoubleField"
    getDoubleField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> IO CDouble

foreign import ccall safe "ffijni.h getBooleanField"
    getBooleanField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> IO Bool

foreign import ccall safe "ffijni.h getCharField"
    getCharField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> IO Word16

foreign import ccall safe "ffijni.h getObjectField"
    getObjectField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> IO (Ptr JObjectRef)

foreign import ccall safe "ffijni.h getStringField"
    getStringField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> IO CString

foreign import ccall safe "ffijni.h setStaticLongField"
    setStaticLongField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> Int64 -> IO ()

foreign import ccall safe "ffijni.h setStaticIntField"
    setStaticIntField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> Int32 -> IO ()

foreign import ccall safe "ffijni.h setStaticShortField"
    setStaticShortField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> Int16 -> IO ()

foreign import ccall safe "ffijni.h setStaticByteField"
    setStaticByteField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> Int8 -> IO ()

foreign import ccall safe "ffijni.h setStaticFloatField"
    setStaticFloatField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> CFloat -> IO ()

foreign import ccall safe "ffijni.h setStaticDoubleField"
    setStaticDoubleField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> CDouble -> IO ()

foreign import ccall safe "ffijni.h setStaticBooleanField"
    setStaticBooleanField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> Bool -> IO ()

foreign import ccall safe "ffijni.h setStaticCharField"
    setStaticCharField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> Word16 -> IO ()

foreign import ccall safe "ffijni.h setStaticObjectField"
    setStaticObjectField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> Ptr JObjectRef -> IO ()

foreign import ccall safe "ffijni.h setStaticStringField"
    setStaticStringField :: Ptr JVM -> Ptr JClassRef -> Ptr JStaticFieldID -> CString -> IO ()

foreign import ccall safe "ffijni.h setLongField"
    setLongField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> Int64 -> IO ()

foreign import ccall safe "ffijni.h setIntField"
    setIntField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> Int32 -> IO ()

foreign import ccall safe "ffijni.h setShortField"
    setShortField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> Int16 -> IO ()

foreign import ccall safe "ffijni.h setByteField"
    setByteField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> Int8 -> IO ()

foreign import ccall safe "ffijni.h setFloatField"
    setFloatField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> CFloat -> IO ()

foreign import ccall safe "ffijni.h setDoubleField"
    setDoubleField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> CDouble -> IO ()

foreign import ccall safe "ffijni.h setBooleanField"
    setBooleanField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> Bool -> IO ()

foreign import ccall safe "ffijni.h setCharField"
    setCharField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> Word16 -> IO ()

foreign import ccall safe "ffijni.h setObjectField"
    setObjectField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> Ptr JObjectRef -> IO ()

foreign import ccall safe "ffijni.h setStringField"
    setStringField :: Ptr JVM -> Ptr JObjectRef -> Ptr JFieldID -> CString -> IO ()

foreign import ccall safe "ffijni.h newJValues"
    newJValues :: CInt -> IO (Ptr JValues)

foreign import ccall safe "ffijni.h setJValueLong"
    setJValueLong :: Ptr JValues -> CInt -> Int64 -> IO ()

foreign import ccall safe "ffijni.h setJValueInt"
    setJValueInt :: Ptr JValues -> CInt -> Int32 -> IO ()

foreign import ccall safe "ffijni.h setJValueShort"
    setJValueShort :: Ptr JValues -> CInt -> Int16 -> IO ()

foreign import ccall safe "ffijni.h setJValueByte"
    setJValueByte :: Ptr JValues -> CInt -> Int8 -> IO ()

foreign import ccall safe "ffijni.h setJValueFloat"
    setJValueFloat :: Ptr JValues -> CInt -> CFloat -> IO ()

foreign import ccall safe "ffijni.h setJValueDouble"
    setJValueDouble :: Ptr JValues -> CInt -> CDouble -> IO ()

foreign import ccall safe "ffijni.h setJValueBoolean"
    setJValueBoolean :: Ptr JValues -> CInt -> Bool -> IO ()

foreign import ccall safe "ffijni.h setJValueChar"
    setJValueChar :: Ptr JValues -> CInt -> Word16 -> IO ()

foreign import ccall safe "ffijni.h setJValueObject"
    setJValueObject :: Ptr JValues -> CInt -> Ptr JObjectRef -> IO ()

foreign import ccall safe "ffijni.h setJValueString"
    setJValueString :: Ptr JVM -> Ptr JValues -> CInt -> CString -> IO ()

-- foreign import ccall safe "ffijni.h newBooleanArray"
--     newBooleanArray :: Ptr JVM -> Int32 -> Ptr JObjectRef

-- foreign import ccall safe "ffijni.h newCharArray"
--     newCharArray :: Ptr JVM -> Int32 -> Ptr JObjectRef

-- foreign import ccall safe "ffijni.h newLongArray"
--     newLongArray :: Ptr JVM -> Int32 -> Ptr JObjectRef

-- foreign import ccall safe "ffijni.h newIntArray"
--     newIntArray :: Ptr JVM -> Int32 -> Ptr JObjectRef

-- foreign import ccall safe "ffijni.h newShortArray"
--     newShortArray :: Ptr JVM -> Int32 -> Ptr JObjectRef

-- foreign import ccall safe "ffijni.h newByteArray"
--     newByteArray :: Ptr JVM -> Int32 -> Ptr JObjectRef

-- foreign import ccall safe "ffijni.h newFloatArray"
--     newFloatArray :: Ptr JVM -> Int32 -> Ptr JObjectRef

-- foreign import ccall safe "ffijni.h newDoubleArray"
--     newDoubleArray :: Ptr JVM -> Int32 -> Ptr JObjectRef

-- foreign import ccall safe "ffijni.h newObjectArray"
--     newObjectArray :: Ptr JVM -> Int32 -> Ptr JClass -> Ptr JObjectRef

foreign import ccall safe "ffijni.h newJString"
    newJString :: Ptr JVM -> CString -> IO (Ptr JObjectRef)

foreign import ccall safe "ffijni.h charsFromJString"
    charsFromJString :: Ptr JVM -> Ptr JObjectRef -> IO (Ptr JChars)

foreign import ccall safe "ffijni.h bytesFromJString"
    bytesFromJString :: Ptr JVM -> Ptr JObjectRef -> IO (Ptr JBytes)

foreign import ccall safe "ffijni.h releaseJChars"
    releaseJChars :: Ptr JVM -> Ptr JObjectRef -> CString -> IO ()

foreign import ccall safe "ffijni.h releaseJBytes"
    releaseJBytes :: Ptr JVM -> Ptr JObjectRef -> CString -> IO ()

foreign import ccall safe "ffijni.h jStringToCString"
    jStringToCString :: Ptr JVM -> Ptr JObjectRef -> CString -> IO ()

foreign import ccall safe "ffijni.h getArrayLength"
    getArrayLength :: Ptr JVM -> Ptr JObjectRef -> IO Int32

foreign import ccall safe "ffijni.h getObjectClass"
    getObjectClass :: Ptr JVM -> Ptr JObjectRef -> IO (Ptr JClassRef)

foreign import ccall safe "ffijni.h isInstanceOf"
    isInstanceOf :: Ptr JVM -> Ptr JObjectRef -> Ptr JClassRef -> IO Bool

-- | Returns the path to libjvm that is used by this library.
-- Note that this will return the @nullPtr@ if the library has not
-- yet been initialized. The library is lazily initialized the first
-- time that 'runJava'' is used. 'runJava' is implemented in terms of
-- 'runJava', as is 'initJava'.
--
-- You are not allowed to invoke 'free' on the returned cstring.
foreign import ccall safe "ffijni.h getLibjvmPath"
    getLibjvmPath :: IO CString

-- | Returns the path to libjvm with which this library has been
-- compiled. This is guaranteed to never return 'nullPtr'.
--
-- You are not allowed to invoke 'free' on the returned cstring.
foreign import ccall safe "ffijni.h getCompiledLibjvmPath"
    getCompiledLibjvmPath :: IO CString

-- | Sets the path to libjvm. Note that this will only have an
-- effect if the library has not yet been initialized, that is
-- before any of the following functions is used: 'runJava',
-- 'runJava'', and 'initJava'.
--
-- Do not invoke 'free' on the cstring passed to this function,
-- as this function will not set a copy but the object given.
-- It is only ever used once during the lifecycle of your
-- application.
foreign import ccall safe "ffijni.h setLibjvmPath"
    setLibjvmPath :: CString -> IO ()

-- | Checks whether an exception has occured in the virtual
-- machine or not.
foreign import ccall safe "ffijni.h exceptionCheck"
    exceptionCheck :: Ptr JVM -> IO Bool

foreign import ccall safe "ffijni.h exceptionClear"
    exceptionClear :: Ptr JVM -> IO ()

foreign import ccall safe "ffijni.h exceptionDescribe"
    exceptionDescribe :: Ptr JVM -> IO ()

-- | Checks whether an exception occured and return that exception.
-- If no exception occured, the returned pointer will be the 'nullPtr'.
-- This method will return a local reference only, so beware that the
-- obtained Ptr will not be valid for too long.
foreign import ccall safe "ffijni.h exceptionOccurred"
    exceptionOccurred :: Ptr JVM -> IO (Ptr JThrowableRef)

-- | Checks whether an exception occured and return that exception.
-- If no exception occured, the returned pointer will be the 'nullPtr'.
-- This method will also call exceptionClear which means that it can
-- not be called twice. This method will return a global reference
-- - as opposed to 'exceptionOccurred', which will return a local
-- reference only.
foreign import ccall safe "ffijni.h exceptionOccurredClear"
    exceptionOccurredClear :: Ptr JVM -> IO (Ptr JThrowableRef)

foreign import ccall safe "ffijni.h releaseJObjectRef"
    releaseJObjectRef :: Ptr JVM -> Ptr JObjectRef -> IO ()

foreign import ccall safe "ffijni.h releaseJClassRef"
    releaseJClassRef :: Ptr JVM -> Ptr JClassRef -> IO ()

foreign import ccall safe "ffijni.h releaseJThrowableRef"
    releaseJThrowableRef :: Ptr JVM -> Ptr JThrowableRef -> IO ()

foreign import ccall safe "ffijni.h &release"
    release_ :: FunPtr (Ptr a -> IO ())

class ReleaseGlobalReference a where
    release :: FunPtr (Ptr a -> IO ())
    -- ^ @release@ is a special function which can be used to create a
    -- 'ForeignPtr'. A ForeignPtr does not carry a reference to a virtual
    -- machine (no @Ptr JVM@), thus this function will lookup the current
    -- virtual machine or do nothing if it can not find one. It is
    -- realised as a 'FunPtr' as this is what 'newForeignPtr' expects.
    release = release_

instance ReleaseGlobalReference JObjectRef
instance ReleaseGlobalReference JClassRef
instance ReleaseGlobalReference JThrowableRef

foreign import ccall safe "ffijni.h registerCallbacks"
    registerCallbacks :: Ptr JVM -> Ptr JClassRef -> IO Bool

foreign import ccall safe "ffijni.h getDebugStatus"
    getDebugStatus :: IO Bool

foreign import ccall safe "ffijni.h setDebugStatus"
    setDebugStatus :: Bool -> IO ()

foreign import ccall safe "ffijni.h runCocoaMain"
    runCocoaMain :: IO ()

