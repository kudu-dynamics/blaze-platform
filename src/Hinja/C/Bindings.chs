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
import Hinja.C.Enums
import Hinja.C.Pointers
import Hinja.C.Structs
import Hinja.C.Types
import Hinja.C.Util
import Hinja.Types
import Hinja.MLIL.Types
import System.IO.Unsafe (unsafePerformIO)

#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#include "/tmp/beauty/binaryninjacore.h"

{#context lib="binaryninjacore" #}

{#fun BNGetBinaryViewTypeName as getBinaryViewTypeName {withPtr* `BNBinaryViewType'} -> `String' #}

{#fun BNUpdateAnalysisAndWait as updateAnalysisAndWait {withPtr* `BNBinaryView'} -> `()' #}

{#fun BNUpdateAnalysis as updateAnalysis {withPtr* `BNBinaryView'} -> `()' #}

{#fun BNGetBinaryViewTypesForData as getBinaryViewTypesForData' {withPtr* `BNBinaryView', alloca- `CSize' peekIntConv*} -> `List (Ptr BNBinaryViewType)' ptrListOut #}

{#fun BNFreeBinaryViewTypeList as freeBinaryViewTypeList {ptrListIn `List (Ptr BNBinaryViewType)'} -> `()' #}

{#fun BNGetAnalysisFunctionList as getAnalysisFunctionList' {withPtr* `BNBinaryView', alloca- `CSize' peekIntConv*} -> `List (Ptr BNFunction)' ptrListOut #}

{#fun BNFreeFunctionList as freeFunctionList {ptrListIn `List (Ptr BNFunction)', `CULong'} -> `()' #}

{#fun BNNewFunctionReference as newFunctionReference {withPtr* `BNFunction'} -> `BNFunction' safePtr* #}

{#fun BNCreateFileMetadata as createFileMetadata {} -> `BNFileMetadata' safePtr* #}

{#fun BNGetFileViewOfType as getFileViewOfType {withPtr* `BNFileMetadata', `String'} -> `Maybe BNBinaryView' nilable* #}

{#fun BNCreateBinaryViewOfType as createBinaryViewOfType {withPtr* `BNBinaryViewType', withPtr* `BNBinaryView'} -> `BNBinaryView' safePtr* #}

{#fun BNSetFilename as setFilename {withPtr* `BNFileMetadata', `String'} -> `()' #}

{#fun BNCreateBinaryDataViewFromFilename as createBinaryDataViewFromFilename {withPtr* `BNFileMetadata', `String'} -> `Maybe BNBinaryView' nilable* #}

{#fun BNSetBundledPluginDirectory as setBundledPluginDirectory {`String'} -> `()' #}

{#fun BNInitCorePlugins as initCorePlugins {} -> `()' #}

{#fun BNInitUserPlugins as initUserPlugins {} -> `()' #}

{#fun BNInitRepoPlugins as initRepoPlugins {} -> `()' #}

{#fun BNIsLicenseValidated as isLicenseValidated {} -> `Bool' toBool #}

{#fun BNOpenExistingDatabase as openExistingDatabase {withPtr* `BNFileMetadata', `String'} -> `Maybe BNBinaryView' nilable* #}


--------- functions

{#fun BNGetFunctionStart as getFunctionStart {withPtr* `BNFunction'} -> `Address' fromIntegral #}

{#fun BNGetFunctionSymbol as getFunctionSymbol {withPtr* `BNFunction'} -> `BNSymbol' safePtr* #}

{#fun BNGetFunctionLowLevelIL as getFunctionLowLevelIL {withPtr* `BNFunction'} -> `BNLowLevelILFunction' safePtr* #}

{#fun BNGetLowLevelILSSAForm as getLowLevelILSSAForm {withPtr* `BNLowLevelILFunction'} -> `BNLowLevelILFunction' safePtr* #}

{#fun BNGetFunctionMediumLevelIL as getFunctionMediumLevelIL {withPtr* `BNFunction'} -> `BNMediumLevelILFunction' safePtr* #}

{#fun BNGetMediumLevelILSSAForm as getMediumLevelILSSAForm {withPtr* `BNMediumLevelILFunction'} -> `BNMediumLevelILFunction' safePtr* #}
--------- symbols

{#fun BNGetSymbolRawName as getSymbolRawName {withPtr* `BNSymbol'} -> `String' #}

{#fun BNGetSymbolFullName as getSymbolFullName {withPtr* `BNSymbol'} -> `String' #}

{#fun BNGetSymbolShortName as getSymbolShortName {withPtr* `BNSymbol'} -> `String' #}

--- architecture

{#fun BNGetAllArchitectureSemanticFlagClasses as getAllArchitectureSemanticFlagClasses' {withPtr* `BNArchitecture', alloca- `CSize' peekIntConv*} -> `List CUInt' id #}

{#fun BNGetArchitectureSemanticFlagClassName as getArchitectureSemanticFlagClassName {withPtr* `BNArchitecture', `Word32'} -> `String' #}

{#fun BNGetAllArchitectureSemanticFlagGroups as getAllArchitectureSemanticFlagGroups' {withPtr* `BNArchitecture', alloca- `CSize' peekIntConv*} -> `List CUInt' id #}

{#fun BNGetArchitectureSemanticFlagGroupName as getArchitectureSemanticFlagGroupName {withPtr* `BNArchitecture', `Word32'} -> `String' #}

{#fun BNGetArchitectureName as getArchitectureName {withPtr* `BNArchitecture'} -> `String' #}

{#fun BNLowLevelILFreeOperandList as lowLevelILFreeOperandList {id `List CULong'} -> `()' #}


---------- basic blocks

{#fun BNGetBasicBlockStart as getBasicBlockStart {withPtr* `BNBasicBlock'} -> `InstructionIndex ()' fromIntegral #}

{#fun BNGetBasicBlockEnd as getBasicBlockEnd {withPtr* `BNBasicBlock'} -> `InstructionIndex ()' fromIntegral #}

{#fun BNGetBasicBlockFunction as getBasicBlockFunction {withPtr* `BNBasicBlock'} -> `BNFunction' safePtr* #}

{#fun BNGetFunctionBasicBlockList as getFunctionBasicBlockList' {withPtr* `BNFunction', alloca- `CSize' peekIntConv*} -> `List (Ptr BNBasicBlock)' ptrListOut #}

{#fun BNGetMediumLevelILBasicBlockList as getMediumLevelILBasicBlockList' {withPtr* `BNMediumLevelILFunction', alloca- `CSize' peekIntConv*} -> `List (Ptr BNBasicBlock)' ptrListOut #}

{#fun BNGetLowLevelILBasicBlockList as getLowLevelILBasicBlockList' {withPtr* `BNLowLevelILFunction', alloca- `CSize' peekIntConv*} -> `List (Ptr BNBasicBlock)' ptrListOut #}


{#fun BNFreeBasicBlockList as freeBasicBlockList {ptrListIn `List (Ptr BNBasicBlock)', `CULong'} -> `()' #}

{#fun BNNewBasicBlockReference as newBasicBlockReference {withPtr* `BNBasicBlock'} -> `BNBasicBlock' safePtr* #}

{#fun BNGetBasicBlocksForAddress as getBasicBlocksForAddress' {withPtr* `BNBinaryView', fromIntegral `Address',  alloca- `CSize' peekIntConv*} -> `List (Ptr BNBasicBlock)' ptrListOut #}

{#fun BNGetLowLevelILBasicBlockForInstruction as getLowLevelILBasicBlockForInstruction {withPtr* `BNLowLevelILFunction', fromIntegral `InstructionIndex LLILFunction'} -> `Maybe BNBasicBlock' nilable* #}

{#fun BNGetMediumLevelILBasicBlockForInstruction as getMediumLevelILBasicBlockForInstruction {withPtr* `BNMediumLevelILFunction', fromIntegral `InstructionIndex MLILFunction'} -> `Maybe BNBasicBlock' nilable* #}


----- MLIL

{#fun BNGetMediumLevelILInstructionCount as getMediumLevelILInstructionCount {withPtr* `BNMediumLevelILFunction'} -> `Word64' #}

{#fun BNGetMediumLevelILIndexForInstruction as getMediumLevelILIndexForInstruction {withPtr* `BNMediumLevelILFunction', fromIntegral `InstructionIndex ()'} -> `ExpressionIndex MLILFunction' fromIntegral #}

{#fun BNGetMediumLevelILSSAExprIndex as getMediumLevelILSSAExprIndex {withPtr* `BNMediumLevelILFunction', fromIntegral `ExpressionIndex MLILFunction'} -> `ExpressionIndex MLILSSAFunction' fromIntegral #}

sizeOfBNMediumLevelILInstruction :: Int
sizeOfBNMediumLevelILInstruction = {#sizeof BNMediumLevelILInstruction#}

sizeOfBNMediumLevelILOperation :: Int
sizeOfBNMediumLevelILOperation = {#sizeof BNMediumLevelILOperation#}

-- allocaMediumLevelILInstruction :: 

#c
void wrapBNGetMediumLevelILByIndex(BNMediumLevelILFunction* func, size_t i, BNMediumLevelILInstruction* instr) {
  *instr = BNGetMediumLevelILByIndex(func, i);
}
#endc

{#fun wrapBNGetMediumLevelILByIndex as wrapBNGetMediumLevelILByIndex {withPtr* `BNMediumLevelILFunction', fromIntegral `ExpressionIndex ()', castPtr `Ptr MediumLevelILInstruction'} -> `()' #}



---- variables

#c
void wrapBNFromVariableIdentifier(uint64_t id, BNVariable* var) {
  *var = BNFromVariableIdentifier(id);
}
#endc

{#fun wrapBNFromVariableIdentifier as wrapBNFromVariableIdentifier {fromIntegral `VariableIdentifier', castPtr `Ptr BNVariable'} -> `()' #}

{#fun BNGetVariableName as getVariableName {withPtr* `BNFunction', withStruct* `BNVariable'} -> `String' #}

#c
void wrapBNGetVariableType(BNFunction* func, const BNVariable* var, BNTypeWithConfidence* t) { *t = BNGetVariableType(func, var); }
#endc

{#fun wrapBNGetVariableType as wrapBNGetVariableType {withPtr* `BNFunction', withStruct* `BNVariable', castPtr `Ptr BNTypeWithConfidence'} -> `()' #}

#c
void wrapBNIsTypeSigned(BNType* ty, BNBoolWithConfidence* bc) {
 *bc = BNIsTypeSigned(ty);
}
#endc

{#fun wrapBNIsTypeSigned as wrapBNIsTypeSigned {withPtr* `BNType', castPtr `Ptr BNBoolWithConfidence'} -> `()' #}

#c
void wrapBNIsTypeConst(BNType* ty, BNBoolWithConfidence* bc) {
 *bc = BNIsTypeConst(ty);
}
#endc

{#fun wrapBNIsTypeConst as wrapBNIsTypeConst {withPtr* `BNType', castPtr `Ptr BNBoolWithConfidence'} -> `()' #}
