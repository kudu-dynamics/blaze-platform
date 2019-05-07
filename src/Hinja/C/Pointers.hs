{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module Hinja.C.Pointers where

-- import Hinja.Prelude

import Hinja.C.TH

$(mkPointer "BNBinaryView" "BNFreeBinaryView")
$(mkPointer_ "BNBinaryViewType")
$(mkPointer "BNFileMetadata" "BNFreeFileMetadata")
$(mkPointer "BNFunction" "BNFreeFunction")
$(mkPointer "BNMediumLevelILFunction" "BNFreeMediumLevelILFunction")
$(mkPointer "BNLowLevelILFunction" "BNFreeLowLevelILFunction")
$(mkPointer "BNSymbol" "BNFreeSymbol")
$(mkPointer_ "BNArchitecture")
$(mkPointer "BNBasicBlock" "BNFreeBasicBlock")
$(mkPointer "BNType" "BNFreeType")
