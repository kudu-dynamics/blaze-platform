{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hinja.C.Util where

import Hinja.Prelude
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

type List = Ptr

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

class MaybeNull a where
  maybeNull :: a -> IO (Maybe a)

-- instance MaybeNull (ForeignPtr a) where
--   maybeNull fptr = withForeignPtr fptr $ \ptr ->
--     return $ if nullPtr == ptr then Nothing else Just fptr

nilable :: (PointerWrap a, HasFinalizer a) => Ptr a -> IO (Maybe a)
nilable ptr
  | ptr == nullPtr = return Nothing
  | otherwise = Just . pointerWrap <$> newForeignPtr finalizer ptr

--without finalizer
nilable_ :: (PointerWrap a) => Ptr a -> IO (Maybe a)
nilable_ ptr
  | ptr == nullPtr = return Nothing
  | otherwise = Just . pointerWrap <$> newForeignPtr_ ptr

-- only adds finalizer if not null
safePtr :: (PointerWrap a, HasFinalizer a) => Ptr a -> IO a
safePtr ptr = pointerWrap <$> fPtr ptr
  where
    fPtr = if ptr == nullPtr
      then newForeignPtr_
      else newForeignPtr finalizer

class HasFinalizer a where
  finalizer :: FinalizerPtr a
  
class PointerWrap a where
  pointerWrap :: ForeignPtr a -> a

