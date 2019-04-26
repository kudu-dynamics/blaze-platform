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
import Hinja.C.Types
import Hinja.C.Pointers
import System.IO.Unsafe (unsafePerformIO)


getBinaryViewTypesForData :: BNBinaryView -> IO [BNBinaryViewType]
getBinaryViewTypesForData bv =
  getBinaryViewTypesForData' bv >>= manifestArray standardPtrConv freeBinaryViewTypeList

getFunctions :: BNBinaryView -> IO [BNFunction]
getFunctions bv = 
  getAnalysisFunctionList' bv
  >>= manifestArrayWithFreeSize (newFunctionReference <=< noFinPtrConv) freeFunctionList

getFunctionName :: BNFunction -> IO String
getFunctionName = getFunctionSymbol >=> getSymbolShortName 

getAllArchitectureSemanticFlagClasses :: BNArchitecture -> IO [Word32]
getAllArchitectureSemanticFlagClasses arch =
  getAllArchitectureSemanticFlagClasses' arch >>=
  manifestArray (pure . fromIntegral) (lowLevelILFreeOperandList . castPtr)

getAllArchitectureSemanticFlagGroups :: BNArchitecture -> IO [Word32]
getAllArchitectureSemanticFlagGroups arch =
  getAllArchitectureSemanticFlagGroups' arch >>=
  manifestArray (pure . fromIntegral) (lowLevelILFreeOperandList . castPtr)

getFunctionBasicBlockList :: BNFunction -> IO [BNBasicBlock]
getFunctionBasicBlockList fn = 
  getFunctionBasicBlockList' fn
  >>= manifestArrayWithFreeSize (newBasicBlockReference <=< noFinPtrConv) freeBasicBlockList

getMediumLevelILBasicBlockList :: BNMediumLevelILFunction -> IO [BNBasicBlock]
getMediumLevelILBasicBlockList fn = 
  getMediumLevelILBasicBlockList' fn
  >>= manifestArrayWithFreeSize (newBasicBlockReference <=< noFinPtrConv) freeBasicBlockList

getLowLevelILBasicBlockList :: BNLowLevelILFunction -> IO [BNBasicBlock]
getLowLevelILBasicBlockList fn = 
  getLowLevelILBasicBlockList' fn
  >>= manifestArrayWithFreeSize (newBasicBlockReference <=< noFinPtrConv) freeBasicBlockList

getBasicBlocksForAddress :: BNBinaryView -> Address -> IO [BNBasicBlock]
getBasicBlocksForAddress bv addr = 
  getBasicBlocksForAddress' bv addr
  >>= manifestArrayWithFreeSize (newBasicBlockReference <=< noFinPtrConv) freeBasicBlockList

