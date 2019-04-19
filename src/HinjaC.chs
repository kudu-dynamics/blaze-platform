{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module HinjaC where

import Hinja.Prelude
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (peekCString, CString
                        , withCString)
import Foreign.ForeignPtr ( ForeignPtr
                          , FinalizerPtr
                          , newForeignPtr
                          , withForeignPtr
                          , newForeignPtr_)

#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#include "/tmp/beauty/binaryninjacore.h"

peekIntConv   :: (Storable a, Integral a, Integral b) 
              => Ptr a -> IO b
peekIntConv    = liftM fromIntegral . peek

type List = Ptr

class UnwrapPtr a where
  unwrap :: a -> Ptr a

{#context lib="binaryninjacore" #}

{#pointer *BNBinaryView foreign finalizer BNFreeBinaryView as ^ newtype #}
deriving instance Show BNBinaryView
deriving instance Eq BNBinaryView

{#pointer *BNBinaryViewType foreign newtype #}
deriving instance Show BNBinaryViewType
deriving instance Eq BNBinaryViewType

--   getBinaryViewTypesForData' :: Ptr BNBinaryView -> Ptr CSize -> IO (Ptr BinaryViewType)

-- foreign import ccall unsafe "/tmp/beauty/binaryninjacore.h BNGetBinaryViewTypeByName"
--   getBinaryViewTypeByName' :: CString -> IO BinaryViewType

-- getBinaryViewTypeByName :: String -> IO BinaryViewType
-- getBinaryViewTypeByName s = withCString getBinaryViewTypeByName'

-- foreign import ccall unsafe "/tmp/beauty/binaryninjacore.h BNGetBinaryViewTypeName"
--   getBinaryViewTypeName' :: Ptr BNBinaryViewType -> IO CString

-- getBinaryViewTypeName :: BinaryViewType -> String
-- getBinaryViewTypeName (BinaryViewType ptr) = unsafePerformIO $
--   getBinaryViewTypeName' ptr >>= peekCString

manifestArray :: (ForeignPtr a -> a) -> (Ptr (Ptr a) -> IO ()) -> (Ptr (Ptr a), CSize) -> IO [a]
manifestArray newtypeConstr freeArray (arr, len) = do
  xs <- peekArray (fromIntegral len) arr
  xs' <- mapM newForeignPtr_ xs
  freeArray arr
  return (newtypeConstr <$> xs')


getBinaryViewTypesForData :: BNBinaryView -> IO [BNBinaryViewType]
getBinaryViewTypesForData bv =
  getBinaryViewTypesForData' bv >>= manifestArray BNBinaryViewType freeBinaryViewTypeList

{#fun BNGetBinaryViewTypeName as getBinaryViewTypeName {`BNBinaryViewType'} -> `String' #}

{#fun BNGetBinaryViewTypesForData as getBinaryViewTypesForData' {`BNBinaryView', alloca- `CSize' peekIntConv*} -> `List (Ptr BNBinaryViewType)' id #}

{#fun BNFreeBinaryViewTypeList as freeBinaryViewTypeList {id `List (Ptr BNBinaryViewType)'} -> `()' #}

-- getBinaryViewTypesForData :: BNBinaryView -> IO [BNBinaryViewType]
-- getBinaryViewTypesForData (BNBinaryView fpbv) = do
--   alloca $ \alength -> do
--     arr <- withForeignPtr fpbv $ \pbv -> getBinaryViewTypesForData' pbv alength
--     n <- peek alength
--     xs <- peekArray (fromIntegral n) arr :: IO [Ptr BNBinaryViewType]
--     xs' <- mapM newForeignPtr_ xs
--     freeBinaryViewTypeList' arr
--     return (BNBinaryViewType <$> xs')

{#pointer *BNFileMetadata foreign finalizer BNFreeFileMetadata as ^ newtype #}

{#fun BNCreateFileMetadata as createFileMetadata {} -> `BNFileMetadata' #}

{#fun BNSetBundledPluginDirectory as setBundledPluginDirectory {`String'} -> `()' #}

{#fun BNInitCorePlugins as initCorePlugins {} -> `()' #}

{#fun BNInitUserPlugins as initUserPlugins {} -> `()' #}

{#fun BNInitRepoPlugins as initRepoPlugins {} -> `()' #}

toBool :: CInt -> Bool
toBool 0 = False
toBool _ = True

{#fun BNIsLicenseValidated as isLicenseValidated {} -> `Bool' toBool #}

{#fun BNOpenExistingDatabase as openExistingDatabase {`BNFileMetadata', `String'} -> `BNBinaryView' #}






