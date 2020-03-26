{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module Binja.C.Pointers where

import Binja.C.TH

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
$(mkPointer "BNPlatform" "BNFreePlatform")
$(mkPointer "BNBinaryReader" "BNFreeBinaryReader")