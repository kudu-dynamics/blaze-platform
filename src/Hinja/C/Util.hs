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


manifestArray :: (Ptr a -> IO a) -> (Ptr (Ptr a) -> IO ()) -> (Ptr (Ptr a), CSize) -> IO [a]
manifestArray f freeArray (arr, len) = do
  xs <- peekArray (fromIntegral len) arr
  xs' <- mapM f xs
  freeArray arr
  return xs'

noFinPtrConv :: Pointer a => Ptr a -> IO a
noFinPtrConv = fmap pointerWrap . newForeignPtr_

standardPtrConv :: Pointer a => Ptr a -> IO a
standardPtrConv = fmap pointerWrap . newFPtr
  where
    newFPtr = case pointerFinalizer of
      Nothing -> newForeignPtr_
      Just fin -> newForeignPtr fin

manifestArrayWithFreeSize :: (Ptr a -> IO a) -> (Ptr (Ptr a) -> CULong -> IO ()) -> (Ptr (Ptr a), CSize) -> IO [a]
manifestArrayWithFreeSize f freeArray (arr, len) = do
  xs <- peekArray (fromIntegral len) arr
  xs' <- mapM f xs
  freeArray arr (fromIntegral len)
  return xs'


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
