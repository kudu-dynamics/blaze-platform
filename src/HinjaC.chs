{-# LANGUAGE ForeignFunctionInterface #-}
 -- {-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HinjaC where

-- import Hinja.Prelude hiding (sin)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.ForeignPtr ( ForeignPtr
                          , FinalizerPtr
                          , newForeignPtr
                          , withForeignPtr)

#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#include "/tmp/beauty/binaryninjacore.h"

{#context lib="binaryninjacore" #}

{#pointer *BNBinaryView foreign finalizer BNFreeBinaryView as ^ newtype #}

-- {#pointer *BNBinaryViewType as ^ newtype #}

newtype BNBinaryViewType = BNBinaryViewType (Ptr BNBinaryViewType)
  deriving (Show, Storable)

-- instance Storable BNBinaryViewType where
--   sizeOf _ = {#sizeOf BNBinaryViewType* #} -- ???
--   alignment = {#alignOf BNBinaryViewType* #}
--   peek ptr = do
--     putStrLn "Trying to peek..."
--     return ptr


foreign import ccall safe "/tmp/beauty/binaryninjacore.h BNGetBinaryViewTypesForData"
  getBinaryViewTypesForData' :: Ptr BNBinaryView -> Ptr CSize -> IO (Ptr BNBinaryViewType)

getBinaryViewTypesForData :: BNBinaryView -> IO [BNBinaryViewType]
getBinaryViewTypesForData (BNBinaryView fpbv) = do
  alloca $ \alength -> do
    arr <- withForeignPtr fpbv $ \pbv -> getBinaryViewTypesForData' pbv alength
    n <- peek alength
    peekArray (fromIntegral n) arr


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






