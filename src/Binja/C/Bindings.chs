{-# OPTIONS_GHC -fno-warn-orphans #-}

module Binja.C.Bindings where

import Binja.Prelude

import qualified Prelude as P
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Binja.C.Enums
import Binja.C.Pointers
import Binja.C.Structs ()
import Binja.C.Types
import Binja.C.Util
import Binja.Types.MLIL
import Binja.Types.Function
import Binja.Types.Variable
import Binja.Types.BasicBlock (BNBasicBlockEdge)
import Binja.Types.Reference (BNReferenceSource(BNReferenceSource))

#include <binaryninjacore.h>
  
{#context lib="binaryninjacore" #}

{#fun unsafe BNGetBinaryViewTypeName as getBinaryViewTypeName {withPtr* `BNBinaryViewType'} -> `String' #}

{#fun unsafe BNUpdateAnalysisAndWait as updateAnalysisAndWait {withPtr* `BNBinaryView'} -> `()' #}

{#fun unsafe BNGetFunctionData as getFunctionData {withPtr* `BNFunction'} -> `BNBinaryView' safePtr* #}

{#fun unsafe BNUpdateAnalysis as updateAnalysis {withPtr* `BNBinaryView'} -> `()' #}

{#fun unsafe BNGetBinaryViewTypesForData as getBinaryViewTypesForData' {withPtr* `BNBinaryView', alloca- `CSize' peekIntConv*} -> `List (Ptr BNBinaryViewType)' ptrListOut #}

{#fun unsafe BNFreeBinaryViewTypeList as freeBinaryViewTypeList {ptrListIn `List (Ptr BNBinaryViewType)'} -> `()' #}

{#fun unsafe BNSaveToFilename as saveToFilename {withPtr* `BNBinaryView', `String'} -> `Bool' toBool #}

{#fun unsafe BNCreateDatabase as createDatabase {withPtr* `BNBinaryView', `String'} -> `Bool' toBool #}

{#fun unsafe BNGetAnalysisFunctionList as getAnalysisFunctionList' {withPtr* `BNBinaryView', alloca- `CSize' peekIntConv*} -> `List (Ptr BNFunction)' ptrListOut #}

{#fun unsafe BNFreeFunctionList as freeFunctionList {ptrListIn `List (Ptr BNFunction)', `Word64'} -> `()' #}

{#fun unsafe BNNewFunctionReference as newFunctionReference {withPtr* `BNFunction'} -> `BNFunction' safePtr* #}

{#fun unsafe BNGetFileForView as getFileForView {withPtr* `BNBinaryView'} -> `BNFileMetadata' safePtr* #}

{#fun unsafe BNCreateFileMetadata as createFileMetadata {} -> `BNFileMetadata' safePtr* #}

{#fun unsafe BNGetFileViewOfType as getFileViewOfType {withPtr* `BNFileMetadata', `String'} -> `Maybe BNBinaryView' nilable* #}

{#fun unsafe BNCreateBinaryViewOfType as createBinaryViewOfType {withPtr* `BNBinaryViewType', withPtr* `BNBinaryView'} -> `BNBinaryView' safePtr* #}

{#fun unsafe BNGetDefaultPlatform as getDefaultPlatform {withPtr* `BNBinaryView'} -> `BNPlatform' safePtr* #}

{#fun unsafe BNSetFilename as setFilename {withPtr* `BNFileMetadata', `String'} -> `()' #}

{#fun unsafe BNCreateBinaryDataViewFromFilename as createBinaryDataViewFromFilename {withPtr* `BNFileMetadata', `String'} -> `Maybe BNBinaryView' nilable* #}

{#fun unsafe BNGetStartOffset as getStartOffset {withPtr* `BNBinaryView'} -> `Address' fromIntegral #}

{#fun unsafe BNSetBundledPluginDirectory as setBundledPluginDirectory {`String'} -> `()' #}

{#fun unsafe BNInitCorePlugins as initCorePlugins {} -> `()' #}

{#fun unsafe BNInitUserPlugins as initUserPlugins {} -> `()' #}

{#fun unsafe BNInitRepoPlugins as initRepoPlugins {} -> `()' #}

{#fun unsafe BNIsLicenseValidated as isLicenseValidated {} -> `Bool' toBool #}

{#fun unsafe BNOpenExistingDatabase as openExistingDatabase {withPtr* `BNFileMetadata', `String'} -> `Maybe BNBinaryView' nilable* #}

---- Stream reader

{#fun unsafe BNCreateBinaryReader as createBinaryReader {withPtr* `BNBinaryView'} -> `Maybe BNBinaryReader' nilable* #}

{#fun unsafe BNFreeBinaryReader as freeBinaryReader {withPtr* `BNBinaryReader'} -> `()' #}

{#fun unsafe BNGetBinaryReaderEndianness as getBinaryReaderEndianness {withPtr* `BNBinaryReader'} -> `BNEndianness' integralToEnum #}

{#fun unsafe BNSetBinaryReaderEndianness as setBinaryReaderEndianness {withPtr* `BNBinaryReader', enumToIntegral `BNEndianness'} -> `()' #}

{#fun unsafe BNReadData as readData' {withPtr* `BNBinaryReader', castPtr `List Word8', `Word64'} -> `Bool' toBool #}

{#fun unsafe BNRead8 as read8' {withPtr* `BNBinaryReader', alloca- `Word8' peekIntConv*} -> `Bool' toBool #}

{#fun unsafe BNRead16 as read16' {withPtr* `BNBinaryReader', alloca- `Word16' peekIntConv*} -> `Bool' toBool #}

{#fun unsafe BNRead32 as read32' {withPtr* `BNBinaryReader', alloca- `Word32' peekIntConv*} -> `Bool' toBool #}

{#fun unsafe BNRead64 as read64' {withPtr* `BNBinaryReader', alloca- `Word64' peekIntConv*} -> `Bool' toBool #}

{#fun unsafe BNReadLE16 as readLE16' {withPtr* `BNBinaryReader', alloca- `Word16' peekIntConv*} -> `Bool' toBool #}

{#fun unsafe BNReadLE32 as readLE32' {withPtr* `BNBinaryReader', alloca- `Word32' peekIntConv*} -> `Bool' toBool #}

{#fun unsafe BNReadLE64 as readLE64' {withPtr* `BNBinaryReader', alloca- `Word64' peekIntConv*} -> `Bool' toBool #}

{#fun unsafe BNReadBE16 as readBE16' {withPtr* `BNBinaryReader', alloca- `Word16' peekIntConv*} -> `Bool' toBool #}

{#fun unsafe BNReadBE32 as readBE32' {withPtr* `BNBinaryReader', alloca- `Word32' peekIntConv*} -> `Bool' toBool #}

{#fun unsafe BNReadBE64 as readBE64' {withPtr* `BNBinaryReader', alloca- `Word64' peekIntConv*} -> `Bool' toBool #}

{#fun unsafe BNGetReaderPosition as getReaderPosition {withPtr* `BNBinaryReader'} -> `Word64' #}

{#fun unsafe BNSeekBinaryReader as seekBinaryReader {withPtr* `BNBinaryReader', `Word64'} -> `()' #}

{#fun unsafe BNSeekBinaryReader as seekBinaryReaderRelative {withPtr* `BNBinaryReader', `Word64'} -> `()' #}

{#fun unsafe BNIsEndOfFile as isEndOfFile {withPtr* `BNBinaryReader'} -> `Bool' toBool #}

--------- functions

{#fun unsafe BNGetFunctionStart as getFunctionStart {withPtr* `BNFunction'} -> `Address' fromIntegral #}

{#fun unsafe BNGetFunctionSymbol as getFunctionSymbol {withPtr* `BNFunction'} -> `BNSymbol' safePtr* #}

{#fun unsafe BNGetFunctionLowLevelIL as getFunctionLowLevelIL {withPtr* `BNFunction'} -> `BNLowLevelILFunction' safePtr* #}

{#fun unsafe BNGetLowLevelILSSAForm as getLowLevelILSSAForm {withPtr* `BNLowLevelILFunction'} -> `BNLowLevelILFunction' safePtr* #}

{#fun unsafe BNGetFunctionMediumLevelIL as getFunctionMediumLevelIL {withPtr* `BNFunction'} -> `BNMediumLevelILFunction' safePtr* #}

{#fun unsafe BNGetMediumLevelILSSAForm as getMediumLevelILSSAForm {withPtr* `BNMediumLevelILFunction'} -> `BNMediumLevelILFunction' safePtr* #}

{#fun unsafe BNGetAnalysisFunction as getGetAnalysisFunction {withPtr* `BNBinaryView', withPtr* `BNPlatform', fromIntegral `Address'} -> `Maybe BNFunction' nilable* #}


--------- symbols

{#fun unsafe BNGetSymbolRawName as getSymbolRawName {withPtr* `BNSymbol'} -> `String' #}

{#fun unsafe BNGetSymbolFullName as getSymbolFullName {withPtr* `BNSymbol'} -> `String' #}

{#fun unsafe BNGetSymbolShortName as getSymbolShortName {withPtr* `BNSymbol'} -> `String' #}

--- architecture

{#fun unsafe BNGetAllArchitectureSemanticFlagClasses as getAllArchitectureSemanticFlagClasses' {withPtr* `BNArchitecture', alloca- `CSize' peekIntConv*} -> `List CUInt' id #}

{#fun unsafe BNGetArchitectureSemanticFlagClassName as getArchitectureSemanticFlagClassName {withPtr* `BNArchitecture', `Word32'} -> `String' #}

{#fun unsafe BNGetAllArchitectureSemanticFlagGroups as getAllArchitectureSemanticFlagGroups' {withPtr* `BNArchitecture', alloca- `CSize' peekIntConv*} -> `List CUInt' id #}

{#fun unsafe BNGetArchitectureSemanticFlagGroupName as getArchitectureSemanticFlagGroupName {withPtr* `BNArchitecture', `Word32'} -> `String' #}

{#fun unsafe BNGetArchitectureName as getArchitectureName {withPtr* `BNArchitecture'} -> `String' #}

{#fun unsafe BNLowLevelILFreeOperandList as lowLevelILFreeOperandList {castPtr `List Word64'} -> `()' #}


---------- basic blocks

{#fun unsafe BNGetBasicBlockStart as getBasicBlockStart {withPtr* `BNBasicBlock'} -> `InstructionIndex ()' fromIntegral #}

{#fun unsafe BNGetBasicBlockEnd as getBasicBlockEnd {withPtr* `BNBasicBlock'} -> `InstructionIndex ()' fromIntegral #}

{#fun unsafe BNGetBasicBlockFunction as getBasicBlockFunction {withPtr* `BNBasicBlock'} -> `BNFunction' safePtr* #}

{#fun unsafe BNGetFunctionBasicBlockList as getFunctionBasicBlockList' {withPtr* `BNFunction', alloca- `CSize' peekIntConv*} -> `List (Ptr BNBasicBlock)' ptrListOut #}

{#fun unsafe BNGetMediumLevelILBasicBlockList as getMediumLevelILBasicBlockList' {withPtr* `BNMediumLevelILFunction', alloca- `CSize' peekIntConv*} -> `List (Ptr BNBasicBlock)' ptrListOut #}

{#fun unsafe BNGetLowLevelILBasicBlockList as getLowLevelILBasicBlockList' {withPtr* `BNLowLevelILFunction', alloca- `CSize' peekIntConv*} -> `List (Ptr BNBasicBlock)' ptrListOut #}


{#fun unsafe BNFreeBasicBlockList as freeBasicBlockList {ptrListIn `List (Ptr BNBasicBlock)', `Word64'} -> `()' #}

{#fun unsafe BNNewBasicBlockReference as newBasicBlockReference {withPtr* `BNBasicBlock'} -> `BNBasicBlock' safePtr* #}

{#fun unsafe BNGetBasicBlocksForAddress as getBasicBlocksForAddress' {withPtr* `BNBinaryView', fromIntegral `Address',  alloca- `CSize' peekIntConv*} -> `List (Ptr BNBasicBlock)' ptrListOut #}

{#fun unsafe BNGetLowLevelILBasicBlockForInstruction as getLowLevelILBasicBlockForInstruction {withPtr* `BNLowLevelILFunction', fromIntegral `InstructionIndex LLILFunction'} -> `Maybe BNBasicBlock' nilable* #}

{#fun unsafe BNGetMediumLevelILBasicBlockForInstruction as getMediumLevelILBasicBlockForInstruction {withPtr* `BNMediumLevelILFunction', fromIntegral `InstructionIndex MLILFunction'} -> `Maybe BNBasicBlock' nilable* #}

{#fun unsafe BNGetBasicBlockOutgoingEdges as getBasicBlockOutgoingEdges' {withPtr* `BNBasicBlock', alloca- `CSize' peekIntConv*} -> `List BNBasicBlockEdge' castPtr #}

{#fun unsafe BNFreeBasicBlockEdgeList as freeBasicBlockEdgeList {castPtr `List BNBasicBlockEdge', `Word64'} -> `()' #}

{#fun unsafe BNGetBasicBlockDominators as getBasicBlockDominators' {withPtr* `BNBasicBlock', alloca- `CSize' peekIntConv*, `Bool'} -> `List (Ptr BNBasicBlock)' ptrListOut #}

----- LLIL

{#fun unsafe BNGetLowLevelILForInstruction as getLowLevelILForInstruction {withPtr* `BNFunction', withPtr* `BNArchitecture', fromIntegral `Address'} -> `InstructionIndex LLILFunction' fromIntegral #}

----- MLIL

{#fun unsafe BNGetMediumLevelILInstructionIndex as getMediumLevelILInstructionIndexFromLLIL {withPtr* `BNLowLevelILFunction', fromIntegral `InstructionIndex LLILFunction'} -> `InstructionIndex MLILFunction' fromIntegral #}

{#fun unsafe BNGetMediumLevelILSSAInstructionIndex as getMediumLevelILSSAInstructionIndexFromMLIL {withPtr* `BNMediumLevelILFunction', fromIntegral `InstructionIndex MLILFunction'} -> `InstructionIndex MLILSSAFunction' fromIntegral #}


{#fun unsafe BNGetMediumLevelILInstructionCount as getMediumLevelILInstructionCount {withPtr* `BNMediumLevelILFunction'} -> `Word64' #}

{#fun unsafe BNGetMediumLevelILIndexForInstruction as getMediumLevelILIndexForInstruction {withPtr* `BNMediumLevelILFunction', fromIntegral `InstructionIndex ()'} -> `ExpressionIndex MLILFunction' fromIntegral #}

{#fun unsafe BNGetMediumLevelILSSAExprIndex as getMediumLevelILSSAExprIndex {withPtr* `BNMediumLevelILFunction', fromIntegral `ExpressionIndex MLILFunction'} -> `ExpressionIndex MLILSSAFunction' fromIntegral #}

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

{#fun unsafe wrapBNGetMediumLevelILByIndex as wrapBNGetMediumLevelILByIndex {withPtr* `BNMediumLevelILFunction', fromIntegral `ExpressionIndex ()', castPtr `Ptr MediumLevelILInstruction'} -> `()' #}


{#fun unsafe BNMediumLevelILGetOperandList as mediumLevelILGetOperandList' {withPtr* `BNMediumLevelILFunction', fromIntegral `ExpressionIndex ()', fromIntegral `OpIndex', alloca- `CSize' peekIntConv*} -> `List Word64' castPtr #}

{#fun unsafe BNMediumLevelILFreeOperandList as mediumLevelILFreeOperandList {castPtr `List Word64'} -> `()' #}

---- variables

#c
void wrapBNFromVariableIdentifier(uint64_t id, BNVariable* var) {
  *var = BNFromVariableIdentifier(id);
}
#endc

{#fun unsafe wrapBNFromVariableIdentifier as wrapBNFromVariableIdentifier {fromIntegral `VariableIdentifier', castPtr `Ptr BNVariable'} -> `()' #}

{#fun unsafe BNGetVariableName as getVariableName {withPtr* `BNFunction', withStruct* `BNVariable'} -> `String' #}

#c
void wrapBNGetChildType(BNType* t, BNTypeWithConfidence* tc) { *tc = BNGetChildType(t); }
#endc

{#fun unsafe wrapBNGetChildType as wrapBNGetChildType {withPtr* `BNType', castPtr `Ptr BNTypeWithConfidence'} -> `()' #}


#c
void wrapBNGetVariableType(BNFunction* func, const BNVariable* var, BNTypeWithConfidence* t) { *t = BNGetVariableType(func, var); }
#endc

{#fun unsafe wrapBNGetVariableType as wrapBNGetVariableType {withPtr* `BNFunction', withStruct* `BNVariable', castPtr `Ptr BNTypeWithConfidence'} -> `()' #}

#c
void wrapBNIsTypeSigned(BNType* ty, BNBoolWithConfidence* bc) {
 *bc = BNIsTypeSigned(ty);
}
#endc

{#fun unsafe wrapBNIsTypeSigned as wrapBNIsTypeSigned {withPtr* `BNType', castPtr `Ptr BNBoolWithConfidence'} -> `()' #}

#c
void wrapBNIsTypeConst(BNType* ty, BNBoolWithConfidence* bc) {
 *bc = BNIsTypeConst(ty);
}
#endc

{#fun unsafe wrapBNIsTypeConst as wrapBNIsTypeConst {withPtr* `BNType', castPtr `Ptr BNBoolWithConfidence'} -> `()' #}

{#fun unsafe BNGetTypeAlignment as getTypeAlignment {withPtr* `BNType'} -> `TypeAlignment' fromIntegral #}

{#fun unsafe BNGetTypeWidth as getTypeWidth {withPtr* `BNType'} -> `TypeWidth' fromIntegral #}

{#fun unsafe BNGetTypeClass as getTypeClass {withPtr* `BNType'} -> `BNTypeClass' integralToEnum #}

{#fun unsafe BNGetTypeString as getTypeString {withPtr* `BNType', withNilablePtr* `Maybe BNPlatform'} -> `String' #}
--------------------------
---------Code References

instance Storable BNReferenceSource where
  sizeOf _ = {#sizeof BNReferenceSource#}
  alignment _ = {#alignof BNReferenceSource#}
  peek p = BNReferenceSource
    -- TODO: can func be null?
    <$> ({#get BNReferenceSource->func #} p >>= noFinPtrConv . castPtr >>= newFunctionReference)
    --- TODO: the arch doesn't need to be free? can it be null?
    <*> ({#get BNReferenceSource->arch #} p >>= noFinPtrConv . castPtr) -- safePtr)
    <*> liftM fromIntegral ({#get BNReferenceSource->addr #} p)
  poke _ _ = P.error "BNReferenceSource 'poke' not implemented"


{#fun unsafe BNGetCodeReferences as getCodeReferences'' {withPtr* `BNBinaryView', fromIntegral `Address', alloca- `CSize' peekIntConv*} -> `List BNReferenceSource' castPtr #}

{#fun unsafe BNFreeCodeReferences as freeCodeReferences {castPtr `List BNReferenceSource', `Word64'} -> `()' #}

