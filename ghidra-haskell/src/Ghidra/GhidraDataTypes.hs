{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}

module Ghidra.GhidraDataTypes
    ( module Ghidra.GhidraDataTypes
    , module Exports
    ) where

import Ghidra.Prelude hiding (replace, Symbol)
import Ghidra.Types.Internal (Ghidra, runIO)
import Ghidra.Util (iteratorToList)
import Ghidra.Program (withTransaction)
import Ghidra.Types.GhidraDataTypes as Exports
import Data.Text (replace)
import Data.Type.Equality
import Foreign.JNI.Types (JClass)
import Language.Java (J)
import Control.Monad.Extra (findM)
import qualified Language.Java as Java
import qualified Ghidra.Types as J


type CycleTracker = [J.DataType]

checkIfCycle :: J.DataType -> CycleTracker -> Ghidra Bool
checkIfCycle dt = foldM (\isCycle currDt -> do
                           isEqual :: Bool <- runIO (Java.call currDt "isEquivalent" dt)
                           return $ isEqual || isCycle) False

parseToOtherType :: J.DataType -> Ghidra GhidraDataType
parseToOtherType dt = do
    width :: Int32 <- runIO $ Java.call dt "getLength"
    name :: Text <- runIO $ Java.call dt "getName" >>= Java.reify
    let dtName = stripModuleName name
    jClass :: JClass <- runIO $ Java.call dt "getClass"
    className :: Text <- runIO $ Java.call jClass "getName" >>= Java.reify
    return $ OtherType OtherTypeOpts { width = fromIntegral width, name = dtName, className = className}

parseRecType :: CycleTracker -> J.DataType -> Ghidra GhidraDataType
parseRecType cycleTracker dt = do
    isCycle <- checkIfCycle dt cycleTracker
    case isCycle of
        False -> parseDataType (dt : cycleTracker) dt 
        True -> parseToOtherType dt


stripModuleName :: Text -> Text
stripModuleName = replace "ghidra.program.model.data." "" 

parseDataTypeWithTransaction :: J.ProgramDB -> J.DataType -> Ghidra GhidraDataType
parseDataTypeWithTransaction prog dt = withTransaction prog "parsing Ghidra datatype" $ parseDataType [] dt

parseDataType :: CycleTracker -> J.DataType -> Ghidra GhidraDataType
parseDataType cycleTracker dt = do
    dataTypeClass :: JClass <- runIO $ Java.call dt "getClass"
    name :: Text <- runIO $ Java.call dataTypeClass "getName" >>= Java.reify
    let dtName = stripModuleName name
    parseDataType' dtName cycleTracker dt


parseDataType' :: Text -> CycleTracker -> J.DataType -> Ghidra GhidraDataType
parseDataType' "Float2DataType"     _ dt = getFloatType dt
parseDataType' "Float4DataType"     _ dt = getFloatType dt
parseDataType' "Float8DataType"     _ dt = getFloatType dt
parseDataType' "Float10DataType"    _ dt = getFloatType dt
parseDataType' "Float16DataType"    _ dt = getFloatType dt
parseDataType' "FloatDataType"      _ dt = getFloatType dt
parseDataType' "DoubleDataType"     _ dt = getFloatType dt
parseDataType' "LongDoubleDataType" _ dt = getFloatType dt

parseDataType' "Integer3DataType"           _ dt = getIntType dt
parseDataType' "Integer5DataType"           _ dt = getIntType dt
parseDataType' "Integer6DataType"           _ dt = getIntType dt
parseDataType' "Integer7DataType"           _ dt = getIntType dt
parseDataType' "Integer16DataType"          _ dt = getIntType dt
parseDataType' "IntegerDataType"            _ dt = getIntType dt
parseDataType' "UnsignedInteger3DataType"   _ dt = getIntType dt
parseDataType' "UnsignedInteger5DataType"   _ dt = getIntType dt
parseDataType' "UnsignedInteger6DataType"   _ dt = getIntType dt
parseDataType' "UnsignedInteger7DataType"   _ dt = getIntType dt
parseDataType' "UnsignedInteger16DataType"  _ dt = getIntType dt
parseDataType' "UnsignedIntegerDataType"    _ dt = getIntType dt
parseDataType' "LongDataType"               _ dt = getIntType dt
parseDataType' "LongLongDataType"           _ dt = getIntType dt
parseDataType' "UnsignedLongDataType"       _ dt = getIntType dt
parseDataType' "UnsignedLongLongDataType"   _ dt = getIntType dt
parseDataType' "ShortDataType"              _ dt = getIntType dt
parseDataType' "UnsignedShortDataType"      _ dt = getIntType dt
parseDataType' "DWordDataType"              _ dt = getIntType dt
parseDataType' "QWordDataType"              _ dt = getIntType dt
parseDataType' "SignedDWordDataType"        _ dt = getIntType dt
parseDataType' "SignedQWordDataType"        _ dt = getIntType dt
parseDataType' "SignedWordDataType"         _ dt = getIntType dt
parseDataType' "WordDataType"               _ dt = getIntType dt

parseDataType' "StringDataType" _ dt = do
    let dt' :: J.StringDataType = coerce dt
    len :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ StringType StringTypeOpts { len = fromIntegral len }
parseDataType' "ArrayDataType" cycleTracker dt = do
    let dt' :: J.ArrayDataType = coerce dt
    dt'' :: J.DataType <- runIO $ Java.call dt' "getDataType"
    elementType <- parseRecType cycleTracker dt''
    elementWidth :: Int32 <- runIO $ Java.call dt' "getElementLength"
    len :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ ArrayType ArrayTypeOpts { elementType = elementType, elementWidth = fromIntegral elementWidth, len = fromIntegral len }
parseDataType' "ghidra.program.database.data.ArrayDB" cycleTracker dt = do
    let dt' :: J.ArrayDB = coerce dt
    dt'' :: J.DataType <- runIO $ Java.call dt' "getDataType"
    elementType :: GhidraDataType <- parseRecType cycleTracker dt''
    elementWidth :: Int32 <- runIO $ Java.call dt' "getElementLength"
    len :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ ArrayType ArrayTypeOpts { elementType = elementType, elementWidth = fromIntegral elementWidth, len = fromIntegral len }
parseDataType' "BadDataType" _ dt = do
    let dt' :: J.BadDataType = coerce dt
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ BadBoyType BadBoyTypeOpts { width = fromIntegral width }
parseDataType' "BooleanDataType" _ dt = do
    let dt' :: J.BooleanDataType = coerce dt
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ BoolType BoolTypeOpts { width = fromIntegral width }
parseDataType' "CharDataType" _ dt = do
    let dt' :: J.CharDataType = coerce dt
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ CharType CharTypeOpts { width = fromIntegral width }
parseDataType' "EnumDataType" _ dt = do
    let dt' :: J.EnumDataType = coerce dt
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    names :: [Text] <- runIO $ Java.call dt' "getNames" >>= Java.reify
    values :: [Int32] <- runIO $ Java.call dt' "getValues" >>= Java.reify
    return $ EnumType EnumTypeOpts { width = fromIntegral width, enums = zip names values }

parseDataType' "Pointer8DataType"   cyclTrac dt = getPointerType cyclTrac dt
parseDataType' "Pointer16DataType"  cyclTrac dt = getPointerType cyclTrac dt
parseDataType' "Pointer24DataType"  cyclTrac dt = getPointerType cyclTrac dt
parseDataType' "Pointer32DataType"  cyclTrac dt = getPointerType cyclTrac dt
parseDataType' "Pointer40DataType"  cyclTrac dt = getPointerType cyclTrac dt
parseDataType' "Pointer48DataType"  cyclTrac dt = getPointerType cyclTrac dt
parseDataType' "Pointer56DataType"  cyclTrac dt = getPointerType cyclTrac dt
parseDataType' "Pointer64DataType"  cyclTrac dt = getPointerType cyclTrac dt
parseDataType' "PointerDataType"    cyclTrac dt = getPointerType cyclTrac dt

parseDataType' "ghidra.program.database.data.PointerDB" cycleTracker dt = do
    let dt' :: J.PointerDB = coerce dt
    {- TODO: currently experiencing deadlock issues -}
    pointeeDt :: J.DataType <- runIO $ Java.call dt' "getDataType"
    pointeeType :: GhidraDataType <- parseRecType cycleTracker pointeeDt
    --pointeeType :: GhidraDataType <- parseDataType cycleTracker pointeeDt 
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ PointerType PointerTypeOpts { width = fromIntegral width, pointeeType = pointeeType {- UnknownType -} } -- it deadlocks when you try to get the actual datatype

parseDataType' "StructureDataType" cycleTracker dt = do
    let dt' :: J.StructureDataType = coerce dt
    fields :: [(Int32,GhidraDataType)] <- getDataTypeComponentsWithOffsets cycleTracker dt'
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ StructType StructTypeOpts { width = fromIntegral width, fields = fields }
parseDataType' "ghidra.program.database.data.StructureDB" cycleTracker dt = do
    let dt' :: J.StructureDB = coerce dt
    fields :: [(Int32,GhidraDataType)] <- getDataTypeComponentsWithOffsets cycleTracker dt'
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ StructType StructTypeOpts { width = fromIntegral width, fields = fields }
parseDataType' "UnionDataType" cycleTracker dt = do
    let dt' :: J.UnionDataType = coerce dt
    datatypes :: [GhidraDataType] <- getDataTypeComponents cycleTracker dt'
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ UnionType UnionTypeOpts { width = fromIntegral width, types = datatypes }
parseDataType' "FunctionDefinitionDataType" cycleTracker dt = do
    let dt' :: J.FunctionDefinitionDataType = coerce dt
    funcParams :: [J.ParameterDefinition] <- runIO $ Java.call dt' "getArguments" >>= Java.reify
    funcParams' :: [FuncParam] <- mapM (getFuncParamType cycleTracker) funcParams
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    callingConvention :: Text <- runIO $ Java.call dt' "getCallingConventionName" >>= Java.reify
    hasNoReturn :: Bool <- runIO $ Java.call dt' "hasNoReturn"
    returnType :: GhidraDataType <- do
        if hasNoReturn then return $ VoidType VoidTypeOpts
        else do
            returnType' :: J.DataType <- runIO $ Java.call dt' "getReturnType"
            returnType'' :: GhidraDataType <- parseRecType cycleTracker returnType'
            return returnType''
    return $ FuncDefType FuncDefTypeOpts { width = fromIntegral width
                                         , funcParams = funcParams'
                                         , callingConvention = callingConvention
                                         , returnType = returnType
                                         }


parseDataType' "Undefined" _ dt = getUndefType dt
parseDataType' "Undefined1DataType" _ dt = getUndefType dt
parseDataType' "Undefined2DataType" _ dt = getUndefType dt
parseDataType' "Undefined3DataType" _ dt = getUndefType dt
parseDataType' "Undefined4DataType" _ dt = getUndefType dt
parseDataType' "Undefined5DataType" _ dt = getUndefType dt
parseDataType' "Undefined6DataType" _ dt = getUndefType dt
parseDataType' "Undefined7DataType" _ dt = getUndefType dt
parseDataType' "Undefined8DataType" _ dt = getUndefType dt

parseDataType' "VoidDataType" _  _ = return $ VoidType VoidTypeOpts 

parseDataType' _ _ dt = do
    width :: Int32 <- runIO $ Java.call dt "getLength"
    name :: Text <- runIO $ Java.call dt "getName" >>= Java.reify
    let dtName = stripModuleName name 
    jClass :: JClass <- runIO $ Java.call dt "getClass"
    className :: Text <- runIO $ Java.call jClass "getName" >>= Java.reify
    return $ OtherType OtherTypeOpts { width = fromIntegral width, name = dtName, className = className}


{- ### Helper Function ### -}

getFuncParamType :: CycleTracker -> J.ParameterDefinition -> Ghidra FuncParam
getFuncParamType cycleTracker param = do
    dt :: J.DataType <- runIO $ Java.call param "getDataType"
    dataType :: GhidraDataType <- parseDataType cycleTracker dt
    name :: Text <- runIO $ Java.call param "getName" >>= Java.reify
    return $ FuncParam { dataType = dataType, name = name}

getUndefType :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra GhidraDataType
getUndefType dt = do
    width :: Int32 <- runIO $ Java.call dt "getLength"
    return $ UndefType UndefTypeOpts { width = fromIntegral width } 

getIntType :: J.DataType -> Ghidra GhidraDataType
getIntType dt = do
    let dt' :: J.AbstractIntegerDataType = coerce dt
    signed :: Bool <- runIO $ Java.call dt' "isSigned"
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ IntType IntTypeOpts { width = fromIntegral width, isSigned = signed }

getFloatType :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra GhidraDataType
getFloatType dt = do
    width :: Int32 <- runIO $ Java.call dt "getLength"
    return $ FloatType FloatTypeOpts { width = fromIntegral width }

getDataTypeComponentsWithOffsets :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => CycleTracker -> a -> Ghidra [(Int32, GhidraDataType)]
getDataTypeComponentsWithOffsets cycleTracker dt = do
    components :: [J.DataTypeComponent] <- runIO $ Java.call dt "getDefinedComponents" >>= Java.reify -- TODO: doesn't handle filler, make it so it handles filler
    dts :: [J.DataType] <-  mapM (\n -> runIO $ Java.call n "getDataType") components
    offsets :: [Int32] <- mapM (\n -> runIO $ Java.call n "getOffset") components
    datatypes :: [GhidraDataType] <- mapM (parseRecType cycleTracker) dts
    return $ zip offsets datatypes

getDataTypeComponents :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => CycleTracker -> a -> Ghidra [GhidraDataType]
getDataTypeComponents cycleTracker dt = do
    components :: [J.DataTypeComponent] <- runIO $ Java.call dt "getDefinedComponents" >>= Java.reify -- TODO: handle fillers
    dts :: [J.DataType] <-  mapM (\n -> runIO $ Java.call n "getDataType") components
    datatypes :: [GhidraDataType] <- mapM (parseRecType cycleTracker) dts
    return datatypes

getPointerType :: CycleTracker -> J.DataType -> Ghidra GhidraDataType
getPointerType cycleTracker dt = do
    let dt' :: J.PointerDataType = coerce dt
    dt'' :: J.DataType <- runIO $ Java.call dt' "getDataType"
    pointeeType :: GhidraDataType <- parseRecType cycleTracker dt''
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ PointerType PointerTypeOpts { width = fromIntegral width, pointeeType = pointeeType }

type Symbol = Text

getDataTypeFromSymAndHighFunc :: J.ProgramDB -> Symbol -> J.HighFunction -> Ghidra (Maybe GhidraDataType)
getDataTypeFromSymAndHighFunc prog symbol hFunc = do
  maybeHSym <- getSymbolFromHighFunc hFunc symbol
  case maybeHSym of
    Nothing -> return Nothing
    Just hSym -> do
        dt :: J.DataType <- runIO $ Java.call hSym "getDataType"
        Just <$> parseDataTypeWithTransaction prog dt

getSymbolFromHighFunc :: J.HighFunction -> Symbol -> Ghidra (Maybe J.HighSymbol)
getSymbolFromHighFunc hFunc symbol = do
    localSymbols :: [J.HighSymbol] <- getLocalSymbols hFunc
    maybeLocalHighSym <- getMatchingSymbol symbol localSymbols
    case maybeLocalHighSym of
        Nothing -> do
            globalSymbols :: [J.HighSymbol] <- getGlobalSymbols hFunc
            getMatchingSymbol symbol globalSymbols
        highSym -> return highSym


getLocalSymbols :: J.HighFunction -> Ghidra [J.HighSymbol]
getLocalSymbols hFunc = do
    localSymbolMap :: J.LocalSymbolMap <- runIO $ Java.call hFunc "getLocalSymbolMap"
    localSymbolsIter :: J.Iterator J.HighSymbol <- runIO $ Java.call localSymbolMap "getSymbols" >>= Java.reify
    iteratorToList localSymbolsIter

getGlobalSymbols :: J.HighFunction -> Ghidra [J.HighSymbol]
getGlobalSymbols hFunc = do
    globalSymbolMap :: J.GlobalSymbolMap <- runIO $ Java.call hFunc "getGlobalSymbolMap"
    globalSymbolsIter :: J.Iterator J.HighSymbol <- runIO $ Java.call globalSymbolMap "getSymbols" >>= Java.reify
    iteratorToList globalSymbolsIter

getMatchingSymbol :: Symbol -> [J.HighSymbol] -> Ghidra (Maybe J.HighSymbol)
getMatchingSymbol symbol = findM (compareToHighSymbol symbol)

compareToHighSymbol :: Symbol -> J.HighSymbol -> Ghidra Bool
compareToHighSymbol symbol highSym = do
    name :: Text <- runIO $ Java.call highSym "getName" >>= Java.reify
    if symbol == name then return True else return False


