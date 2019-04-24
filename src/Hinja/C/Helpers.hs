{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hinja.C.Helpers where

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
import Hinja.C.Bindings
import Hinja.C.Util
import Hinja.C.Pointers
import System.IO.Unsafe (unsafePerformIO)


getBinaryViewTypesForData :: BNBinaryView -> IO [BNBinaryViewType]
getBinaryViewTypesForData bv =
  getBinaryViewTypesForData' bv >>= manifestArray standardPtrConv freeBinaryViewTypeList

getAnalysisFunctionList :: BNBinaryView -> IO [BNFunction]
getAnalysisFunctionList bv = 
  getAnalysisFunctionList' bv
  >>= manifestArrayWithFreeSize (newFunctionReference <=< noFinPtrConv) freeFunctionList
