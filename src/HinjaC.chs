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

#include <math.h>
#include "/tmp/binaryninjacore.h"


sin :: Double -> Double
sin = realToFrac . {#call pure sin as _sin#} . realToFrac


{#pointer *BNFileMetadata foreign finalizer BNFreeFileMetadata as ^ newtype #}

{#fun BNCreateFileMetadata as ^ {} -> `BNFileMetadata' #}



-- {#fun unsafe BNFreeFileMetadata as ^
--   { `BNFileMetadata' } -> `()' id#}




