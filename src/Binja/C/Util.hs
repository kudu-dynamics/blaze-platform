{-# LANGUAGE NoImplicitPrelude    #-}

module Binja.C.Util where

import Binja.Prelude

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

import qualified Data.Text as T

import Binja.C.Types

manifestArrayWithFree ::
  (Storable a) =>
  (a -> IO b) ->
  (List a -> IO ()) ->
  (List a, CSize) ->
  IO [b]
manifestArrayWithFree f freeArray (arr, len) = do
  xs <- peekArray (fromIntegral len) arr
  xs' <- mapM f xs
  freeArray arr
  return xs'

manifestArrayWithFreeSize ::
  (Storable a) =>
  (a -> IO b) ->
  (List a -> Word64 -> IO ()) ->
  (List a, CSize) ->
  IO [b]
manifestArrayWithFreeSize f freeArray (arr, len) = 
  manifestArrayWithFree f (`freeArray` fromIntegral len) (arr, len)

noFinPtrConv :: Pointer a => Ptr a -> IO a
noFinPtrConv = fmap pointerWrap . newForeignPtr_

standardPtrConv :: Pointer a => Ptr a -> IO a
standardPtrConv = fmap pointerWrap . newFPtr
  where
    newFPtr = maybe newForeignPtr_ newForeignPtr pointerFinalizer

peekIntConv   :: (Storable a, Integral a, Integral b) 
              => Ptr a -> IO b
peekIntConv    = fmap fromIntegral . peek

toBool :: CInt -> Bool
toBool 0 = False
toBool _ = True


allocaStruct :: forall b. Storable b => (Ptr () -> IO (Bool, b)) -> IO (Bool, b)
allocaStruct f = alloca g where
  g :: Ptr b -> IO (Bool, b)
  g = f . castPtr

toStruct :: (Storable a) => Ptr () -> IO a
toStruct ptr = peek ptr'
  where
    ptr' = castPtr ptr

nilable :: (Pointer a) => Ptr () -> IO (Maybe a)
nilable ptr
  | ptr == nullPtr = return Nothing
  | otherwise = Just <$> safePtr ptr

-- no finalizer, but null pointer is Nothing
nilable_ :: (Pointer a) => Ptr () -> IO (Maybe a)
nilable_ ptr
  | ptr == nullPtr = return Nothing
  | otherwise = Just <$> noFinPtrConv (castPtr ptr)

withNilablePtr :: Pointer a => Maybe a -> (Ptr () -> IO b) -> IO b
withNilablePtr Nothing action = do
  fp <- newForeignPtr_ nullPtr
  withForeignPtr fp action
withNilablePtr (Just p) action = withPtr p action

withPtr :: Pointer a => a -> (Ptr () -> IO b) -> IO b
withPtr = withForeignPtr . castForeignPtr . pointerUnwrap

withStruct :: (Storable a) => a -> (Ptr x -> IO b) -> IO b
withStruct s f = alloca $ \ptr -> do
  poke ptr s
  f $ castPtr ptr

withMaybeStruct :: (Storable a) => Maybe a -> (Ptr x -> IO b) -> IO b
withMaybeStruct ms f =
  case ms of
    (Just s) -> withStruct s f
    Nothing -> f nullPtr

allocAndPeek :: Storable b => (Ptr b -> IO ()) -> IO b
allocAndPeek f = alloca $ \ptr -> f ptr >> peek ptr

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

integralToEnum :: (Enum b, Integral a) => a -> b
integralToEnum = toEnum . fromIntegral

enumToIntegral :: (Enum a, Integral b) => a -> b
enumToIntegral = fromIntegral . fromEnum

makeCStringArray :: [Text] -> IO (Ptr CString)
makeCStringArray arr = newArray =<< traverse (newCString . T.unpack) arr

freeCStringArray :: Int -> Ptr CString -> IO ()
freeCStringArray n ptr = do
  strs <- peekArray n ptr
  traverse_ free strs
  free ptr

withCStringArray :: [Text] -> (Ptr CString -> IO a) -> IO a
withCStringArray strs f = do
  arr <- makeCStringArray strs
  result <- f arr
  freeCStringArray (length strs) arr
  return result

peekText :: CString -> IO Text
peekText = fmap T.pack . peekCString
