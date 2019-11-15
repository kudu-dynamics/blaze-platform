module Binja.C.Helpers where

import Binja.Prelude

import Foreign.Ptr
import Binja.C.Bindings
import Binja.C.Util
import Binja.C.Types
import Binja.C.Pointers
import Binja.Types.MLIL
import Binja.Types.Variable
import Binja.Types.BasicBlock (BNBasicBlockEdge)
import Binja.Types.Reference (BNReferenceSource)

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

getMediumLevelILByIndex :: BNMediumLevelILFunction -> ExpressionIndex () -> IO MediumLevelILInstruction
getMediumLevelILByIndex fn eindex =
  allocAndPeek $ wrapBNGetMediumLevelILByIndex fn eindex

fromVariableIdentifier :: VariableIdentifier -> IO BNVariable
fromVariableIdentifier vid =
  allocAndPeek $ wrapBNFromVariableIdentifier vid

getVariableType :: BNFunction -> BNVariable -> IO BNTypeWithConfidence
getVariableType fn var' =
  allocAndPeek $ wrapBNGetVariableType fn var'

isTypeSigned :: BNType -> IO BNBoolWithConfidence
isTypeSigned t =
  allocAndPeek $ wrapBNIsTypeSigned t

isTypeConst :: BNType -> IO BNBoolWithConfidence
isTypeConst t =
  allocAndPeek $ wrapBNIsTypeConst t

mediumLevelILGetOperandList :: BNMediumLevelILFunction -> ExpressionIndex () -> OpIndex -> IO [Int64]
mediumLevelILGetOperandList fn eindex oindex =
  mediumLevelILGetOperandList' fn eindex oindex
  >>= manifestArray (return . fromIntegral) mediumLevelILFreeOperandList

getCodeReferences' :: BNBinaryView -> Address -> IO [BNReferenceSource]
getCodeReferences' bv addr =
  getCodeReferences'' bv addr
  >>= manifestArrayWithFreeSize return freeCodeReferences

getBasicBlockOutgoingEdges :: BNBasicBlock -> IO [BNBasicBlockEdge]
getBasicBlockOutgoingEdges bb = 
  getBasicBlockOutgoingEdges' bb
  >>= manifestArrayWithFreeSize return freeBasicBlockEdgeList

getBasicBlockDominators :: BNBasicBlock -> Bool -> IO [BNBasicBlock]
getBasicBlockDominators bb isPost = 
  getBasicBlockDominators' bb isPost
  >>= manifestArrayWithFreeSize (newBasicBlockReference <=< noFinPtrConv) freeBasicBlockList

