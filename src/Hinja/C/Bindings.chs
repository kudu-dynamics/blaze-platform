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
instance PointerWrap BNBinaryView where
  pointerWrap = BNBinaryView
instance HasFinalizer BNBinaryView where
  finalizer = bNFreeBinaryView
deriving instance Show BNBinaryView
deriving instance Eq BNBinaryView

{#pointer *BNBinaryViewType foreign newtype #}
instance PointerWrap BNBinaryViewType where
  pointerWrap = BNBinaryViewType
deriving instance Show BNBinaryViewType
deriving instance Eq BNBinaryViewType

{#pointer *BNFileMetadata foreign finalizer BNFreeFileMetadata as ^ newtype #}
instance PointerWrap BNFileMetadata where
  pointerWrap = BNFileMetadata
instance HasFinalizer BNFileMetadata where
  finalizer = bNFreeFileMetadata
deriving instance Show BNFileMetadata
deriving instance Eq BNFileMetadata

--   getBinaryViewTypesForData' :: Ptr BNBinaryView -> Ptr CSize -> IO (Ptr BinaryViewType)

-- foreign import ccall unsafe "/tmp/beauty/binaryninjacore.h BNGetBinaryViewTypeByName"
--   getBinaryViewTypeByName' :: CString -> IO BinaryViewType

-- getBinaryViewTypeByName :: String -> IO BinaryViewType
-- getBinaryViewTypeByName s = withCString getBinaryViewTypeByName'

-- foreign import ccall unsafe "/tmp/beauty/binaryninjacore.h BNGetBinaryViewTypeName"
--   getBinaryViewTypeName' :: Ptr BNBinaryViewType -> IO CString

{#fun BNGetBinaryViewTypeName as getBinaryViewTypeName {`BNBinaryViewType'} -> `String' #}

{#fun BNGetBinaryViewTypesForData as getBinaryViewTypesForData' {`BNBinaryView', alloca- `CSize' peekIntConv*} -> `List (Ptr BNBinaryViewType)' id #}

{#fun BNFreeBinaryViewTypeList as freeBinaryViewTypeList {id `List (Ptr BNBinaryViewType)'} -> `()' #}

{#fun BNCreateFileMetadata as createFileMetadata {} -> `BNFileMetadata' #}

{#fun BNGetFileViewOfType as getFileViewOfType {`BNFileMetadata', `String'} -> `Maybe BNBinaryView' nilable* #}

{#fun BNCreateBinaryViewOfType as createBinaryViewOfType {`BNBinaryViewType', `BNBinaryView'} -> `BNBinaryView' #}




{#fun BNSetFilename as setFilename {`BNFileMetadata', `String'} -> `()' #}

{#fun BNCreateBinaryDataViewFromFilename as createBinaryDataViewFromFilename {`BNFileMetadata', `String'} -> `Maybe BNBinaryView' nilable* #}


{#fun BNSetBundledPluginDirectory as setBundledPluginDirectory {`String'} -> `()' #}

{#fun BNInitCorePlugins as initCorePlugins {} -> `()' #}

{#fun BNInitUserPlugins as initUserPlugins {} -> `()' #}

{#fun BNInitRepoPlugins as initRepoPlugins {} -> `()' #}

{#fun BNIsLicenseValidated as isLicenseValidated {} -> `Bool' toBool #}

{#fun BNOpenExistingDatabase as openExistingDatabase {`BNFileMetadata', `String'} -> `BNBinaryView' #}






