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
import Hinja.C.TH

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


nilable :: (Pointer a) => Ptr a -> IO (Maybe a)
nilable ptr
  | ptr == nullPtr = return Nothing
  | otherwise = Just <$> safePtr ptr


-- nilable :: (Pointer a, HasFinalizer a) => Ptr a -> IO (Maybe a)
-- nilable ptr
--   | ptr == nullPtr = return Nothing
--   | otherwise = Just . pointerWrap <$> newForeignPtr finalizer ptr

--without finalizer
-- nilable_ :: (Pointer a) => Ptr a -> IO (Maybe a)
-- nilable_ ptr
--   | ptr == nullPtr = return Nothing
--   | otherwise = Just . pointerWrap <$> newForeignPtr_ ptr

-- only adds finalizer if not null
-- safePtr :: (Pointer a, HasFinalizer a) => Ptr a -> IO a
-- safePtr ptr = pointerWrap <$> fPtr ptr
--   where
--     fPtr = if ptr == nullPtr
--       then newForeignPtr_
--       else newForeignPtr finalizer

class HasFinalizer a where
  finalizer :: FinalizerPtr a
  
class Pointer a where
  pointerWrap :: ForeignPtr a -> a
  pointerUnwrap :: a -> ForeignPtr a
  pointerFinalizer :: Maybe (FinalizerPtr a)

withPtr :: Pointer a => a -> (Ptr a -> IO b) -> IO b
withPtr = withForeignPtr . pointerUnwrap

-- use this for pointers you're sure won't be null
safePtr :: (Pointer a) => Ptr a -> IO a
safePtr ptr = pointerWrap <$> fPtr pointerFinalizer ptr
  where
    fPtr Nothing = newForeignPtr_
    fPtr (Just fin) = if ptr == nullPtr
      then newForeignPtr_
      else newForeignPtr fin


add3 :: Int -> Int
add3 = $(mkAdder 3)

data Jim = Jim
$(derivePointer ''Jim)

$(mkPointer "Billy")
