{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Hinja.C.Util where

import Hinja.Prelude

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Hinja.C.Types


manifestArray :: (ForeignPtr a -> a) -> (Ptr (Ptr a) -> IO ()) -> (Ptr (Ptr a), CSize) -> IO [a]
manifestArray newtypeConstr freeArray (arr, len) = do
  xs <- peekArray (fromIntegral len) arr
  xs' <- mapM newForeignPtr_ xs
  freeArray arr
  return (newtypeConstr <$> xs')

peekIntConv   :: (Storable a, Integral a, Integral b) 
              => Ptr a -> IO b
peekIntConv    = liftM fromIntegral . peek

toBool :: CInt -> Bool
toBool 0 = False
toBool _ = True


nilable :: (Pointer a) => Ptr () -> IO (Maybe a)
nilable ptr
  | ptr == nullPtr = return Nothing
  | otherwise = Just <$> safePtr ptr

withPtr :: Pointer a => a -> (Ptr () -> IO b) -> IO b
withPtr = withForeignPtr . castForeignPtr . pointerUnwrap

-- use this for pointers you're sure won't be null
safePtr :: (Pointer a) => Ptr () -> IO a
safePtr ptr' = pointerWrap <$> fPtr pointerFinalizer ptr
  where
    ptr = castPtr ptr'
    fPtr Nothing = newForeignPtr_
    fPtr (Just fin) = if ptr == nullPtr
      then newForeignPtr_
      else newForeignPtr fin

ptrListOut :: List (Ptr ()) -> List (Ptr a)
ptrListOut = castPtr

ptrListIn :: List (Ptr a) -> List (Ptr ())
ptrListIn = castPtr
