{-# OPTIONS_GHC -fno-warn-orphans #-}

module Binja.C.Bindings where

import Binja.Prelude

import qualified Prelude as P

import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc

import Binja.C.Enums
import Binja.C.Pointers
import Binja.C.Structs ()
import Binja.C.Types
import Binja.C.Util
import Binja.Types.Analysis (BNAnalysisParameters)
import Binja.Types.BasicBlock (BNBasicBlockEdge)
import Binja.Types.Function
import Binja.Types.MLIL
import Binja.Types.Reference (BNReferenceSource(BNReferenceSource))
import Binja.Types.StringReference (BNStringReference)
import Binja.Types.Symbol (BNNameSpace)
import Binja.Types.TypeLibrary (BNQualifiedNameAndType, BNFunctionParameter)
import Binja.Types.Variable

#include <binaryninjacore.h>

{#context lib="binaryninjacore" #}

{#fun BNGetBinaryViewTypeName as getBinaryViewTypeName {withPtr* `BNBinaryViewType'} -> `String' #}

{#fun BNIsFunctionTooLarge as isFunctionTooLarge_ {withPtr* `BNFunction'} -> `Bool' toBool #}

{#fun BNIsFunctionAnalysisSkipped as isFunctionAnalysisSkipped_ {withPtr* `BNFunction'} -> `Bool' toBool #}

{#fun BNGetAnalysisSkipReason as getAnalysisSkipReason_ {withPtr* `BNFunction'} -> `BNAnalysisSkipReason' integralToEnum #}

{#fun BNGetFunctionAnalysisSkipOverride as getFunctionAnalysisSkipOverride_ {withPtr* `BNFunction'} -> `BNFunctionAnalysisSkipOverride' integralToEnum #}

{#fun BNSetFunctionAnalysisSkipOverride as setFunctionAnalysisSkipOverride_ {withPtr* `BNFunction', enumToIntegral `BNFunctionAnalysisSkipOverride'} -> `()' #}

#c
void wrapBNGetParametersForAnalysis(BNBinaryView* view, BNAnalysisParameters* ap) {
  *ap = BNGetParametersForAnalysis(view);
}
#endc

{#fun wrapBNGetParametersForAnalysis as wrapBNGetParametersForAnalysis {withPtr* `BNBinaryView', castPtr `Ptr BNAnalysisParameters'} -> `()' #}

#c
void wrapBNSetParametersForAnalysis(BNBinaryView* view, BNAnalysisParameters* ap) {
  BNSetParametersForAnalysis(view, *ap);
}
#endc

{#fun wrapBNSetParametersForAnalysis as wrapBNSetParametersForAnalysis {withPtr* `BNBinaryView', castPtr `Ptr BNAnalysisParameters'} -> `()' #}

{#fun BNUpdateAnalysisAndWait as updateAnalysisAndWait {withPtr* `BNBinaryView'} -> `()' #}

{#fun BNReanalyzeAllFunctions as reanalyzeAllFunctions {withPtr* `BNBinaryView'} -> `()' #}

{#fun BNGetFunctionData as getFunctionData_ {withPtr* `BNFunction'} -> `BNBinaryView' safePtr* #}

{#fun BNUpdateAnalysis as updateAnalysis {withPtr* `BNBinaryView'} -> `()' #}

{#fun BNGetBinaryViewTypesForData as getBinaryViewTypesForData' {withPtr* `BNBinaryView', alloca- `CSize' peekIntConv*} -> `List (Ptr BNBinaryViewType)' ptrListOut #}

{#fun BNFreeBinaryViewTypeList as freeBinaryViewTypeList {ptrListIn `List (Ptr BNBinaryViewType)'} -> `()' #}

{#fun BNSaveToFilename as saveToFilename {withPtr* `BNBinaryView', `String'} -> `Bool' toBool #}

{#fun BNCreateSaveSettings as createSaveSettings {} -> `BNSaveSettings' safePtr* #}

{#fun BNCreateDatabase as createDatabase' {withPtr* `BNBinaryView', `String', withPtr* `BNSaveSettings'} -> `Bool' toBool #}

{#fun BNGetAnalysisFunctionList as getAnalysisFunctionList' {withPtr* `BNBinaryView', alloca- `CSize' peekIntConv*} -> `List (Ptr BNFunction)' ptrListOut #}

{#fun BNFreeFunctionList as freeFunctionList {ptrListIn `List (Ptr BNFunction)', `Word64'} -> `()' #}

{#fun BNNewFunctionReference as newFunctionReference {withPtr* `BNFunction'} -> `BNFunction' safePtr* #}

{#fun BNGetFileForView as getFileForView {withPtr* `BNBinaryView'} -> `BNFileMetadata' safePtr* #}

{#fun BNCreateFileMetadata as createFileMetadata {} -> `BNFileMetadata' safePtr* #}

{#fun BNGetFileViewOfType as getFileViewOfType {withPtr* `BNFileMetadata', `String'} -> `Maybe BNBinaryView' nilable* #}

{#fun BNCreateBinaryViewOfType as createBinaryViewOfType {withPtr* `BNBinaryViewType', withPtr* `BNBinaryView'} -> `BNBinaryView' safePtr* #}

{#fun BNGetDefaultPlatform as getDefaultPlatform {withPtr* `BNBinaryView'} -> `BNPlatform' safePtr* #}

{#fun BNReadViewBuffer as readViewBuffer {withPtr* `BNBinaryView', fromIntegral `Word64', fromIntegral `Bytes'} -> `BNDataBuffer' safePtr* #}

{#fun BNDuplicateDataBuffer as duplicateDataBuffer {withPtr* `BNDataBuffer'} -> `BNDataBuffer' safePtr* #}

{#fun BNGetDataBufferContents as getDataBufferContents' {withPtr* `BNDataBuffer'} -> `Ptr CChar' castPtr #}

{#fun BNFreeDataBuffer as freeDataBuffer {withPtr* `BNDataBuffer'} -> `()' #}

{#fun BNGetViewLength as getViewLength {withPtr* `BNBinaryView'} -> `Bytes' fromIntegral #}

{#fun BNGetDefaultEndianness as getDefaultEndianness {withPtr* `BNBinaryView'} -> `BNEndianness' integralToEnum #}

{#fun BNSetFilename as setFilename {withPtr* `BNFileMetadata', `String'} -> `()' #}

{#fun BNCreateBinaryDataViewFromFilename as createBinaryDataViewFromFilename {withPtr* `BNFileMetadata', `String'} -> `Maybe BNBinaryView' nilable* #}

{#fun BNGetStartOffset as getStartOffset {withPtr* `BNBinaryView'} -> `Address' fromIntegral #}

{#fun BNSetBundledPluginDirectory as setBundledPluginDirectory {`String'} -> `()' #}

{#fun BNInitCorePlugins as initCorePlugins {} -> `()' #}

{#fun BNInitUserPlugins as initUserPlugins {} -> `()' #}

{#fun BNInitRepoPlugins as initRepoPlugins {} -> `()' #}

{#fun BNIsLicenseValidated as isLicenseValidated {} -> `Bool' toBool #}

{#fun BNOpenExistingDatabase as openExistingDatabase {withPtr* `BNFileMetadata', `String'} -> `Maybe BNBinaryView' nilable* #}

{#fun BNGetViewAddressSize as getViewAddressSize {withPtr* `BNBinaryView' } -> `AddressWidth' bytesToAddressWidth #}

---- Stream reader

{#fun BNCreateBinaryReader as createBinaryReader {withPtr* `BNBinaryView'} -> `Maybe BNBinaryReader' nilable* #}

{#fun BNFreeBinaryReader as freeBinaryReader {withPtr* `BNBinaryReader'} -> `()' #}

{#fun BNGetBinaryReaderEndianness as getBinaryReaderEndianness {withPtr* `BNBinaryReader'} -> `BNEndianness' integralToEnum #}

{#fun BNSetBinaryReaderEndianness as setBinaryReaderEndianness {withPtr* `BNBinaryReader', enumToIntegral `BNEndianness'} -> `()' #}

{#fun BNReadData as readData' {withPtr* `BNBinaryReader', castPtr `List Word8', fromIntegral `Bytes'} -> `Bool' toBool #}

{#fun BNRead8 as read8' {withPtr* `BNBinaryReader', alloca- `Word8' peekIntConv*} -> `Bool' toBool #}

{#fun BNRead16 as read16' {withPtr* `BNBinaryReader', alloca- `Word16' peekIntConv*} -> `Bool' toBool #}

{#fun BNRead32 as read32' {withPtr* `BNBinaryReader', alloca- `Word32' peekIntConv*} -> `Bool' toBool #}

{#fun BNRead64 as read64' {withPtr* `BNBinaryReader', alloca- `Word64' peekIntConv*} -> `Bool' toBool #}

{#fun BNReadLE16 as readLE16' {withPtr* `BNBinaryReader', alloca- `Word16' peekIntConv*} -> `Bool' toBool #}

{#fun BNReadLE32 as readLE32' {withPtr* `BNBinaryReader', alloca- `Word32' peekIntConv*} -> `Bool' toBool #}

{#fun BNReadLE64 as readLE64' {withPtr* `BNBinaryReader', alloca- `Word64' peekIntConv*} -> `Bool' toBool #}

{#fun BNReadBE16 as readBE16' {withPtr* `BNBinaryReader', alloca- `Word16' peekIntConv*} -> `Bool' toBool #}

{#fun BNReadBE32 as readBE32' {withPtr* `BNBinaryReader', alloca- `Word32' peekIntConv*} -> `Bool' toBool #}

{#fun BNReadBE64 as readBE64' {withPtr* `BNBinaryReader', alloca- `Word64' peekIntConv*} -> `Bool' toBool #}

{#fun BNGetReaderPosition as getReaderPosition {withPtr* `BNBinaryReader'} -> `Bytes' fromIntegral #}

{#fun BNSeekBinaryReader as seekBinaryReader {withPtr* `BNBinaryReader', fromIntegral `Bytes'} -> `()' #}

{#fun BNSeekBinaryReaderRelative as seekBinaryReaderRelative {withPtr* `BNBinaryReader', fromIntegral `Bytes'} -> `()' #}

{#fun BNIsEndOfFile as isEndOfFile {withPtr* `BNBinaryReader'} -> `Bool' toBool #}


---- strings

{#fun BNGetStringAtAddress as getStringAtAddress'
  {withPtr* `BNBinaryView', fromIntegral `Address', allocaStruct- `BNStringReference' toStruct*} -> `Bool' toBool #}

{#fun BNGetStrings as getStrings'
  {withPtr* `BNBinaryView', alloca- `CSize' peekIntConv*} -> `List BNStringReference' castPtr #}

{#fun BNFreeStringReferenceList as freeStringReferenceList
  {castPtr `List BNStringReference'} -> `()' #}


---- functions

{#fun BNGetFunctionStart as getFunctionStart {withPtr* `BNFunction'} -> `Address' fromIntegral #}

{#fun BNGetFunctionSymbol as getFunctionSymbol {withPtr* `BNFunction'} -> `BNSymbol' safePtr* #}

{#fun BNGetFunctionLowLevelIL as getFunctionLowLevelIL {withPtr* `BNFunction'} -> `BNLowLevelILFunction' safePtr* #}

{#fun BNGetLowLevelILSSAForm as getLowLevelILSSAForm {withPtr* `BNLowLevelILFunction'} -> `BNLowLevelILFunction' safePtr* #}

{#fun BNGetFunctionMediumLevelIL as getFunctionMediumLevelIL {withPtr* `BNFunction'} -> `BNMediumLevelILFunction' safePtr* #}

{#fun BNGetMediumLevelILSSAForm as getMediumLevelILSSAForm {withPtr* `BNMediumLevelILFunction'} -> `BNMediumLevelILFunction' safePtr* #}

{#fun BNGetAnalysisFunction as getGetAnalysisFunction {withPtr* `BNBinaryView', withPtr* `BNPlatform', fromIntegral `Address'} -> `Maybe BNFunction' nilable* #}

#c
void wrapBNGetFunctionParameterVariables(BNFunction* func, BNParameterVariablesWithConfidence* pvs) {
  *pvs = BNGetFunctionParameterVariables(func);
}
#endc

{#fun wrapBNGetFunctionParameterVariables as wrapBNGetFunctionParameterVariables {withPtr* `BNFunction', castPtr `Ptr BNParameterVariablesWithConfidence'} -> `()' #}

{#fun BNFreeParameterVariables as freeParameterVariables {castPtr `Ptr BNParameterVariablesWithConfidence'} -> `()' #}

{#fun BNGetFunctionType as getFunctionType {withPtr* `BNFunction'} -> `BNType' safePtr* #}

#c
void wrapBNFunctionHasVariableArguments(BNFunction* func, BNBoolWithConfidence* b) {
  *b = BNFunctionHasVariableArguments(func);
}
#endc

{#fun wrapBNFunctionHasVariableArguments as wrapBNFunctionHasVariableArguments {withPtr* `BNFunction', castPtr `Ptr BNBoolWithConfidence'} -> `()' #}

---- symbols

{#fun BNGetSymbolRawName as getSymbolRawName {withPtr* `BNSymbol'} -> `String' #}

{#fun BNGetSymbolFullName as getSymbolFullName {withPtr* `BNSymbol'} -> `String' #}

{#fun BNGetSymbolShortName as getSymbolShortName {withPtr* `BNSymbol'} -> `String' #}

{#fun BNGetSymbolByAddress as getSymbolByAddress {withPtr* `BNBinaryView', fromIntegral `Address', withMaybeStruct* `Maybe BNNameSpace'} -> `Maybe BNSymbol' nilable* #}


---- architecture
{#fun BNGetDefaultArchitecture as getDefaultArchitecture {withPtr* `BNBinaryView'} -> `BNArchitecture' safePtr* #}

{#fun BNGetAllArchitectureSemanticFlagClasses as getAllArchitectureSemanticFlagClasses' {withPtr* `BNArchitecture', alloca- `CSize' peekIntConv*} -> `List CUInt' id #}

{#fun BNGetArchitectureSemanticFlagClassName as getArchitectureSemanticFlagClassName {withPtr* `BNArchitecture', `Word32'} -> `String' #}

{#fun BNGetAllArchitectureSemanticFlagGroups as getAllArchitectureSemanticFlagGroups' {withPtr* `BNArchitecture', alloca- `CSize' peekIntConv*} -> `List CUInt' id #}

{#fun BNGetArchitectureSemanticFlagGroupName as getArchitectureSemanticFlagGroupName {withPtr* `BNArchitecture', `Word32'} -> `String' #}

{#fun BNGetArchitectureName as getArchitectureName {withPtr* `BNArchitecture'} -> `String' #}

{#fun BNLowLevelILFreeOperandList as lowLevelILFreeOperandList {castPtr `List Word64'} -> `()' #}


---- basic blocks

{#fun BNGetBasicBlockStart as getBasicBlockStart {withPtr* `BNBasicBlock'} -> `InstructionIndex ()' fromIntegral #}

{#fun BNGetBasicBlockEnd as getBasicBlockEnd {withPtr* `BNBasicBlock'} -> `InstructionIndex ()' fromIntegral #}

{#fun BNGetBasicBlockFunction as getBasicBlockFunction {withPtr* `BNBasicBlock'} -> `BNFunction' safePtr* #}

{#fun BNGetFunctionBasicBlockList as getFunctionBasicBlockList' {withPtr* `BNFunction', alloca- `CSize' peekIntConv*} -> `List (Ptr BNBasicBlock)' ptrListOut #}

{#fun BNGetMediumLevelILBasicBlockList as getMediumLevelILBasicBlockList' {withPtr* `BNMediumLevelILFunction', alloca- `CSize' peekIntConv*} -> `List (Ptr BNBasicBlock)' ptrListOut #}

{#fun BNGetLowLevelILBasicBlockList as getLowLevelILBasicBlockList' {withPtr* `BNLowLevelILFunction', alloca- `CSize' peekIntConv*} -> `List (Ptr BNBasicBlock)' ptrListOut #}

{#fun BNFreeBasicBlockList as freeBasicBlockList {ptrListIn `List (Ptr BNBasicBlock)', `Word64'} -> `()' #}

{#fun BNNewBasicBlockReference as newBasicBlockReference {withPtr* `BNBasicBlock'} -> `BNBasicBlock' safePtr* #}

{#fun BNGetBasicBlocksForAddress as getBasicBlocksForAddress' {withPtr* `BNBinaryView', fromIntegral `Address',  alloca- `CSize' peekIntConv*} -> `List (Ptr BNBasicBlock)' ptrListOut #}

{#fun BNGetLowLevelILBasicBlockForInstruction as getLowLevelILBasicBlockForInstruction {withPtr* `BNLowLevelILFunction', fromIntegral `InstructionIndex LLILFunction'} -> `Maybe BNBasicBlock' nilable* #}

{#fun BNGetMediumLevelILBasicBlockForInstruction as getMediumLevelILBasicBlockForInstruction {withPtr* `BNMediumLevelILFunction', fromIntegral `InstructionIndex MLILFunction'} -> `Maybe BNBasicBlock' nilable* #}

{#fun BNGetBasicBlockOutgoingEdges as getBasicBlockOutgoingEdges' {withPtr* `BNBasicBlock', alloca- `CSize' peekIntConv*} -> `List BNBasicBlockEdge' castPtr #}

{#fun BNFreeBasicBlockEdgeList as freeBasicBlockEdgeList {castPtr `List BNBasicBlockEdge', `Word64'} -> `()' #}

{#fun BNGetBasicBlockDominators as getBasicBlockDominators' {withPtr* `BNBasicBlock', alloca- `CSize' peekIntConv*, `Bool'} -> `List (Ptr BNBasicBlock)' ptrListOut #}

---- LLIL

{#fun BNGetLowLevelILForInstruction as getLowLevelILForInstruction {withPtr* `BNFunction', withPtr* `BNArchitecture', fromIntegral `Address'} -> `InstructionIndex LLILFunction' fromIntegral #}

---- MLIL

{#fun BNGetMediumLevelILInstructionIndex as getMediumLevelILInstructionIndexFromLLIL {withPtr* `BNLowLevelILFunction', fromIntegral `InstructionIndex LLILFunction'} -> `InstructionIndex MLILFunction' fromIntegral #}

{#fun BNGetMediumLevelILSSAInstructionIndex as getMediumLevelILSSAInstructionIndexFromMLIL {withPtr* `BNMediumLevelILFunction', fromIntegral `InstructionIndex MLILFunction'} -> `InstructionIndex MLILSSAFunction' fromIntegral #}


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


{#fun BNMediumLevelILGetOperandList as mediumLevelILGetOperandList' {withPtr* `BNMediumLevelILFunction', fromIntegral `ExpressionIndex ()', fromIntegral `OpIndex', alloca- `CSize' peekIntConv*} -> `List Word64' castPtr #}

{#fun BNMediumLevelILFreeOperandList as mediumLevelILFreeOperandList {castPtr `List Word64'} -> `()' #}

---- variables

#c
void wrapBNFromVariableIdentifier(uint64_t id, BNVariable* var) {
  *var = BNFromVariableIdentifier(id);
}
#endc

{#fun wrapBNFromVariableIdentifier as wrapBNFromVariableIdentifier {fromIntegral `VariableIdentifier', castPtr `Ptr BNVariable'} -> `()' #}

{#fun BNGetVariableName as getVariableName {withPtr* `BNFunction', withStruct* `BNVariable'} -> `String' #}

#c
void wrapBNGetChildType(BNType* t, BNTypeWithConfidence* tc) {
  *tc = BNGetChildType(t); }
#endc

{#fun wrapBNGetChildType as wrapBNGetChildType {withPtr* `BNType', castPtr `Ptr BNTypeWithConfidence'} -> `()' #}

#c
void wrapBNGetVariableType(BNFunction* func, const BNVariable* var, BNTypeWithConfidence* t) {
  *t = BNGetVariableType(func, var); }
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

{#fun BNGetTypeAlignment as getTypeAlignment {withPtr* `BNType'} -> `TypeAlignment' fromIntegral #}

{#fun BNGetTypeWidth as getTypeWidth {withPtr* `BNType'} -> `TypeWidth' fromIntegral #}

{#fun BNGetTypeClass as getTypeClass {withPtr* `BNType'} -> `BNTypeClass' integralToEnum #}

{#fun BNGetTypeString as getTypeString {withPtr* `BNType', withNilablePtr* `Maybe BNPlatform'} -> `String' #}

---- Code References

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


{#fun BNGetCodeReferences as getCodeReferences'' {withPtr* `BNBinaryView', fromIntegral `Address', alloca- `CSize' peekIntConv*} -> `List BNReferenceSource' castPtr #}

{#fun BNFreeCodeReferences as freeCodeReferences {castPtr `List BNReferenceSource', `Word64'} -> `()' #}

---- Sections

{#fun BNGetSectionsAt as getSectionsAt' {withPtr* `BNBinaryView', fromIntegral `Address', alloca- `CSize' peekIntConv*} -> `List (Ptr BNSection)' ptrListOut #}

{#fun BNSectionGetType as getSectionType {withPtr* `BNSection'} -> `String' #}

{#fun BNFreeSectionList as freeSectionList {ptrListIn `List (Ptr BNSection)', `Word64'} -> `()' #}

{#fun BNFreeSection as freeSection {withPtr* `BNSection'} -> `()' #}

{#fun BNNewSectionReference as newSectionReference {withPtr* `BNSection'} -> `BNSection' safePtr* #}

---- Type Libraries

{#fun BNFreeTypeLibrary as freeTypeLibrary {withPtr* `BNTypeLibrary'} -> `()' #}

{#fun BNNewTypeLibraryReference as newTypeLibraryReference {withPtr* `BNTypeLibrary'} -> `BNTypeLibrary' safePtr* #}

{#fun BNLoadTypeLibraryFromFile as loadTypeLibraryFromFile' {`String'} -> `BNTypeLibrary' safePtr* #}

{#fun BNGetTypeLibraryNamedObjects as getTypeLibraryNamedObjects' {withPtr* `BNTypeLibrary', alloca- `CSize' peekIntConv*} -> `List BNQualifiedNameAndType' castPtr #}

{#fun BNFreeQualifiedNameAndType as freeQualifiedNameAndType {castPtr `Ptr BNQualifiedNameAndType'} -> `()' #}

{#fun BNFreeQualifiedNameAndTypeArray as freeQualifiedNameAndTypeArray {castPtr `List BNQualifiedNameAndType', `Word64'} -> `()' #}

{#fun BNGetTypeParameters as getTypeParameters' {withPtr* `BNType', alloca- `CSize' peekIntConv*} -> `List BNFunctionParameter' castPtr #}

{#fun BNFreeTypeParameterList as freeTypeParameterList {castPtr `List BNFunctionParameter', `Word64'} -> `()' #}

{#fun BNNewTypeReference as newTypeReference {withPtr* `BNType'} -> `BNType' safePtr* #}

{#fun BNGetBinaryViewTypeLibraries as getBinaryViewTypeLibraries' {withPtr* `BNBinaryView', alloca- `CSize' peekIntConv*} -> `List (Ptr BNTypeLibrary)' ptrListOut #}

{#fun BNFreeTypeLibraryList as freeTypeLibraryList {ptrListIn `List (Ptr BNTypeLibrary)', `Word64'} -> `()' #}

{#fun BNGetTypeLibraryName as getTypeLibraryName {withPtr* `BNTypeLibrary'} -> `String' #}
