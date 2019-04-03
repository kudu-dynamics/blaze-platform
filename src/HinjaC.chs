{-# LANGUAGE ForeignFunctionInterface #-}
 -- {-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings #-}

module HinjaC where

-- import Hinja.Prelude hiding (sin)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr ( ForeignPtr
                          , FinalizerPtr
                          , newForeignPtr)

#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#include "/tmp/beauty/binaryninjacore.h"

{#context lib="binaryninjacore" #}

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

-- {#fun BNIsUIEnabled as ^ {} -> `Bool' #}




