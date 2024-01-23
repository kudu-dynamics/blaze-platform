module Binja.C.Helpers where

import Binja.Prelude hiding (reader)

import Foreign.Ptr
import Foreign.Marshal.Array (allocaArray, peekArray)

import Binja.C.Bindings
import Binja.C.Util
import Binja.C.Types
import Binja.C.Pointers
import Binja.Types.MLIL
import Binja.Types.Variable
import Binja.Types.BasicBlock (BNBasicBlockEdge)
import Binja.Types.Reference (BNReferenceSource)
import Binja.Types.StringReference (BNStringReference)
import qualified Binja.Types.TypeLibrary as TypeLib
import Binja.Types.TypeLibrary (BNQualifiedNameAndType, BNFunctionParameter)
import Binja.C.Structs ()
import Foreign.Storable (peek)
import Foreign.Marshal.Alloc (alloca)
import qualified Data.ByteString as BS


getBinaryViewTypesForData :: BNBinaryView -> IO [BNBinaryViewType]
getBinaryViewTypesForData bv =
  getBinaryViewTypesForData' bv >>= manifestArrayWithFree standardPtrConv freeBinaryViewTypeList

getFunctions :: BNBinaryView -> IO [BNFunction]
getFunctions bv =
  getAnalysisFunctionList' bv
  >>= manifestArrayWithFreeSize (newFunctionReference <=< noFinPtrConv) freeFunctionList

getFunctionName :: BNFunction -> IO String
getFunctionName = getFunctionSymbol >=> getSymbolShortName

getAllArchitectureSemanticFlagClasses :: BNArchitecture -> IO [Word32]
getAllArchitectureSemanticFlagClasses arch =
  getAllArchitectureSemanticFlagClasses' arch >>=
  manifestArrayWithFree (pure . fromIntegral) (lowLevelILFreeOperandList . castPtr)

getAllArchitectureSemanticFlagGroups :: BNArchitecture -> IO [Word32]
getAllArchitectureSemanticFlagGroups arch =
  getAllArchitectureSemanticFlagGroups' arch >>=
  manifestArrayWithFree (pure . fromIntegral) (lowLevelILFreeOperandList . castPtr)

getSectionsAt :: BNBinaryView -> Address -> IO [BNSection]
getSectionsAt bv addr =
  getSectionsAt' bv addr
  >>= manifestArrayWithFreeSize (newSectionReference <=< noFinPtrConv) freeSectionList

loadTypeLibraryFromFile :: String -> IO BNTypeLibrary
loadTypeLibraryFromFile p = loadTypeLibraryFromFile' p >>= newTypeLibraryReference

getBinaryViewTypeLibraries :: BNBinaryView -> IO [BNTypeLibrary]
getBinaryViewTypeLibraries bv = getBinaryViewTypeLibraries' bv 
                                  >>= manifestArrayWithFreeSize (newTypeLibraryReference <=< noFinPtrConv) freeTypeLibraryList

getTypeLibraryNamedObjects :: BNTypeLibrary -> IO [BNQualifiedNameAndType]
getTypeLibraryNamedObjects tl = getTypeLibraryNamedObjects' tl
  >>= manifestArrayWithFreeSize updateBnTypePtrReference freeQualifiedNameAndTypeArray

getTypeParameters :: BNType -> IO [BNFunctionParameter]
getTypeParameters t = getTypeParameters' t
  >>= manifestArrayWithFreeSize updateBnTypePtrReference freeTypeParameterList

updateBnTypePtrReference :: TypeLib.HasBnTypePtr a (Maybe BNType) => a -> IO a
updateBnTypePtrReference x = case x ^. TypeLib.bnTypePtr of
  Nothing -> return x
  (Just t) -> do
    t' <- newTypeReference t
    return $ x & TypeLib.bnTypePtr ?~ t'

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

getFunctionParameterVariables_ :: BNFunction -> IO BNParameterVariablesWithConfidence
getFunctionParameterVariables_ fn = alloca $ \ptr -> do
  wrapBNGetFunctionParameterVariables fn ptr
  r <- peek ptr
  freeParameterVariables ptr
  return r

functionHasVariableArguments_ :: BNFunction -> IO BNBoolWithConfidence 
functionHasVariableArguments_ fn = 
  allocAndPeek $ wrapBNFunctionHasVariableArguments fn
  
getChildType :: BNType -> IO BNTypeWithConfidence
getChildType = allocAndPeek . wrapBNGetChildType

isTypeSigned :: BNType -> IO BNBoolWithConfidence
isTypeSigned t =
  allocAndPeek $ wrapBNIsTypeSigned t

isTypeConst :: BNType -> IO BNBoolWithConfidence
isTypeConst t =
  allocAndPeek $ wrapBNIsTypeConst t

mediumLevelILGetOperandList :: BNMediumLevelILFunction -> ExpressionIndex () -> OpIndex -> IO [Word64]
mediumLevelILGetOperandList fn eindex oindex =
  mediumLevelILGetOperandList' fn eindex oindex
  >>= manifestArrayWithFree (return . fromIntegral) mediumLevelILFreeOperandList

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

readData :: BNBinaryReader -> Bytes -> IO (Maybe [Word8])
readData reader len =
  allocaArray (fromIntegral len) converter
    where
      len' :: Int
      len' = fromIntegral len
      converter :: Ptr Word8 -> IO (Maybe [Word8])
      converter buf = do
        ok <- readData' reader buf len
        if ok then
          Just <$> peekArray len' buf
        else
          pure Nothing

maybeValue :: (Bool, a) -> Maybe a
maybeValue (valid, x) =
  if valid then
    Just x
  else
    Nothing

read8 :: BNBinaryReader -> IO (Maybe Word8)
read8 r = maybeValue <$> read8' r

read16 :: BNBinaryReader -> IO (Maybe Word16)
read16 r = maybeValue <$> read16' r

read32 :: BNBinaryReader -> IO (Maybe Word32)
read32 r = maybeValue <$> read32' r

read64 :: BNBinaryReader -> IO (Maybe Word64)
read64 r = maybeValue <$> read64' r

readLE16 :: BNBinaryReader -> IO (Maybe Word16)
readLE16 r = maybeValue <$> readLE16' r

readLE32 :: BNBinaryReader -> IO (Maybe Word32)
readLE32 r = maybeValue <$> readLE32' r

readLE64 :: BNBinaryReader -> IO (Maybe Word64)
readLE64 r = maybeValue <$> readLE64' r

readBE16 :: BNBinaryReader -> IO (Maybe Word16)
readBE16 r = maybeValue <$> readBE16' r

readBE32 :: BNBinaryReader -> IO (Maybe Word32)
readBE32 r = maybeValue <$> readBE32' r

readBE64 :: BNBinaryReader -> IO (Maybe Word64)
readBE64 r = maybeValue <$> readBE64' r

getStringRefAtAddress :: BNBinaryView -> Address -> IO (Maybe BNStringReference)
getStringRefAtAddress v addr = do
  (success, stringRef) <- getStringAtAddress' v addr
  if success then
    return (Just stringRef)
  else
    return Nothing

getStringRefs :: BNBinaryView -> IO [BNStringReference]
getStringRefs bv =
  getStrings' bv
  >>= manifestArrayWithFree return freeStringReferenceList

createDatabase :: BNBinaryView -> FilePath -> IO Bool
createDatabase bv fp = createSaveSettings >>= createDatabase' bv fp

getDataBufferContents :: BNDataBuffer -> Bytes -> IO ByteString
getDataBufferContents buf len
  = getDataBufferContents' buf >>= BS.packCStringLen . (,fromIntegral len)
