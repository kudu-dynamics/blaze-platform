{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hinja.C.Bindings where

import Hinja.Prelude

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.C.String ( peekCString
                        , CString
                        , withCString)
import Foreign.ForeignPtr ( ForeignPtr
                          , FinalizerPtr
                          , newForeignPtr
                          , withForeignPtr
                          , newForeignPtr_)
--import Hinja.C.Struct
import Hinja.C.Util
import System.IO.Unsafe (unsafePerformIO)

#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#include "/tmp/beauty/binaryninjacore.h"

{#context lib="binaryninjacore" #}

{#pointer *BNBinaryView foreign finalizer BNFreeBinaryView as ^ newtype #}
instance Pointer BNBinaryView where
  pointerWrap = BNBinaryView
  pointerUnwrap (BNBinaryView x) = x
  pointerFinalizer = Just bNFreeBinaryView
deriving instance Show BNBinaryView
deriving instance Eq BNBinaryView

{#pointer *BNBinaryViewType foreign newtype #}
instance Pointer BNBinaryViewType where
  pointerWrap = BNBinaryViewType
  pointerUnwrap (BNBinaryViewType x) = x
  pointerFinalizer = Nothing
deriving instance Show BNBinaryViewType
deriving instance Eq BNBinaryViewType

{#pointer *BNFileMetadata foreign finalizer BNFreeFileMetadata as ^ newtype #}
instance Pointer BNFileMetadata where
  pointerWrap = BNFileMetadata
  pointerUnwrap (BNFileMetadata x) = x
  pointerFinalizer = Just bNFreeFileMetadata
deriving instance Show BNFileMetadata
deriving instance Eq BNFileMetadata


--   getBinaryViewTypesForData' :: Ptr BNBinaryView -> Ptr CSize -> IO (Ptr BinaryViewType)

-- foreign import ccall unsafe "/tmp/beauty/binaryninjacore.h BNGetBinaryViewTypeByName"
--   getBinaryViewTypeByName' :: CString -> IO BinaryViewType

-- getBinaryViewTypeByName :: String -> IO BinaryViewType
-- getBinaryViewTypeByName s = withCString getBinaryViewTypeByName'

-- foreign import ccall unsafe "/tmp/beauty/binaryninjacore.h BNGetBinaryViewTypeName"
--   getBinaryViewTypeName' :: Ptr BNBinaryViewType -> IO CString

{#fun BNGetBinaryViewTypeName as getBinaryViewTypeName {withPtr* `BNBinaryViewType'} -> `String' #}

{#fun BNGetBinaryViewTypesForData as getBinaryViewTypesForData' {withPtr* `BNBinaryView', alloca- `CSize' peekIntConv*} -> `List (Ptr BNBinaryViewType)' id #}

{#fun BNFreeBinaryViewTypeList as freeBinaryViewTypeList {id `List (Ptr BNBinaryViewType)'} -> `()' #}

{#fun BNCreateFileMetadata as createFileMetadata {} -> `BNFileMetadata' #}

{#fun BNGetFileViewOfType as getFileViewOfType {withPtr* `BNFileMetadata', `String'} -> `Maybe BNBinaryView' nilable* #}

{#fun BNCreateBinaryViewOfType as createBinaryViewOfType {withPtr* `BNBinaryViewType', withPtr* `BNBinaryView'} -> `BNBinaryView' safePtr* #}




{#fun BNSetFilename as setFilename {`BNFileMetadata', `String'} -> `()' #}

{#fun BNCreateBinaryDataViewFromFilename as createBinaryDataViewFromFilename {`BNFileMetadata', `String'} -> `Maybe BNBinaryView' nilable* #}


{#fun BNSetBundledPluginDirectory as setBundledPluginDirectory {`String'} -> `()' #}

{#fun BNInitCorePlugins as initCorePlugins {} -> `()' #}

{#fun BNInitUserPlugins as initUserPlugins {} -> `()' #}

{#fun BNInitRepoPlugins as initRepoPlugins {} -> `()' #}

{#fun BNIsLicenseValidated as isLicenseValidated {} -> `Bool' toBool #}

{#fun BNOpenExistingDatabase as openExistingDatabase {withPtr* `BNFileMetadata', `String'} -> `Maybe BNBinaryView' nilable* #}



