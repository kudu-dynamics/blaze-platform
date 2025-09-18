{-# LANGUAGE DerivingStrategies #-}

module Ghidra.DataTypes
    ( module Ghidra.DataTypes
    , module Exports
    ) where

import Ghidra.Prelude hiding (replace, DataType)
import Data.Text (replace)
import qualified Language.Java as Java
import qualified Ghidra.Types as J
import Language.Java (J)
import Ghidra.Types.Internal (Ghidra, runIO)
import Ghidra.Types.DataTypes as Exports
import Data.Type.Equality
import Foreign.JNI.Types (JClass)

{-
data ImpTye
    = Self
    | Unknown
    | ImpType (PilType ImpType)
-}

parseDataType :: J.DataType -> Ghidra DataType
parseDataType dt = do
    dataTypeClass :: JClass <- runIO $ Java.call dt "getClass"
    name :: Text <- runIO $ Java.call dataTypeClass "getName" >>= Java.reify
    let dtName = replace "ghidra.program.model.data." "" name
    parseDataType' dtName dt


parseDataType' :: Text -> J.DataType -> Ghidra DataType
parseDataType' "Float2DataType"     dt = getFloatType dt
parseDataType' "Float4DataType"     dt = getFloatType dt
parseDataType' "Float8DataType"     dt = getFloatType dt
parseDataType' "Float10DataType"    dt = getFloatType dt
parseDataType' "Float16DataType"    dt = getFloatType dt
parseDataType' "FloatDataType"      dt = getFloatType dt
parseDataType' "DoubleDataType"     dt = getFloatType dt
parseDataType' "LongDoubleDataType" dt = getFloatType dt

parseDataType' "Integer3DataType"           dt = getIntType dt
parseDataType' "Integer5DataType"           dt = getIntType dt
parseDataType' "Integer6DataType"           dt = getIntType dt
parseDataType' "Integer7DataType"           dt = getIntType dt
parseDataType' "Integer16DataType"          dt = getIntType dt
parseDataType' "IntegerDataType"            dt = getIntType dt
parseDataType' "UnsignedInteger3DataType"   dt = getIntType dt
parseDataType' "UnsignedInteger5DataType"   dt = getIntType dt
parseDataType' "UnsignedInteger6DataType"   dt = getIntType dt
parseDataType' "UnsignedInteger7DataType"   dt = getIntType dt
parseDataType' "UnsignedInteger16DataType"  dt = getIntType dt
parseDataType' "UnsignedIntegerDataType"    dt = getIntType dt
parseDataType' "LongDataType"               dt = getIntType dt
parseDataType' "LongLongDataType"           dt = getIntType dt
parseDataType' "UnsignedLongDataType"       dt = getIntType dt
parseDataType' "UnsignedLongLongDataType"   dt = getIntType dt
parseDataType' "ShortDataType"              dt = getIntType dt
parseDataType' "UnsignedShortDataType"      dt = getIntType dt
parseDataType' "DWordDataType"              dt = getIntType dt
parseDataType' "QWordDataType"              dt = getIntType dt
parseDataType' "SignedDWordDataType"        dt = getIntType dt
parseDataType' "SignedQWordDataType"        dt = getIntType dt
parseDataType' "SignedWordDataType"         dt = getIntType dt
parseDataType' "WordDataType"               dt = getIntType dt

parseDataType' "StringDataType" dt = do
    let dt' :: J.StringDataType = coerce dt
    len :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ StringType StringTypeOpts { len = fromIntegral len }
parseDataType' "ArrayDataType" dt = do
    let dt' :: J.ArrayDataType = coerce dt
    dt'' :: J.DataType <- runIO $ Java.call dt' "getDataType"
    elementType :: DataType <- parseDataType dt''
    elementWidth :: Int32 <- runIO $ Java.call dt' "getElementLength"
    len :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ ArrayType ArrayTypeOpts { elementType = elementType, elementWidth = fromIntegral elementWidth, len = fromIntegral len }
parseDataType' "BadDataType" dt = do
    let dt' :: J.BadDataType = coerce dt
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ BadBoyType BadBoyTypeOpts { width = fromIntegral width }
parseDataType' "BooleanDataType" dt = do
    let dt' :: J.BooleanDataType = coerce dt
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ BoolType BoolTypeOpts { width = fromIntegral width }
parseDataType' "CharDataType" dt = do
    let dt' :: J.CharDataType = coerce dt
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ CharType CharTypeOpts { width = fromIntegral width }
parseDataType' "EnumDataType" dt = do
    let dt' :: J.EnumDataType = coerce dt
    names :: [Text] <- runIO $ Java.call dt' "getNames" >>= Java.reify
    values :: [Int32] <- runIO $ Java.call dt' "getValues" >>= Java.reify
    return $ EnumType EnumTypeOpts { enums = zip names values }

parseDataType' "Pointer8DataType"   dt = getPointerType dt
parseDataType' "Pointer16DataType"  dt = getPointerType dt
parseDataType' "Pointer24DataType"  dt = getPointerType dt
parseDataType' "Pointer32DataType"  dt = getPointerType dt
parseDataType' "Pointer40DataType"  dt = getPointerType dt
parseDataType' "Pointer48DataType"  dt = getPointerType dt
parseDataType' "Pointer56DataType"  dt = getPointerType dt
parseDataType' "Pointer64DataType"  dt = getPointerType dt
parseDataType' "PointerDataType"    dt = getPointerType dt

parseDataType' "StructureDataType" dt = do
    let dt' :: J.StructureDataType = coerce dt
    fields :: [(Int32,DataType)] <- getDataTypeComponentsWithOffsets dt'
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ StructType StructTypeOpts { width = fromIntegral width, fields = fields }
parseDataType' "UnionDataType" dt = do
    let dt' :: J.UnionDataType = coerce dt
    datatypes :: [DataType] <- getDataTypeComponents dt'
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ UnionType UnionTypeOpts { width = fromIntegral width, types = datatypes }

parseDataType' "FunctionDefinitionDataType" dt = do
    let dt' :: J.FunctionDefinitionDataType = coerce dt
    funcParams :: [J.ParameterDefinition] <- runIO $ Java.call dt' "getArguments" >>= Java.reify
    funcParams' :: [FuncParam] <- mapM getFuncParamType funcParams

    width :: Int32 <- runIO $ Java.call dt' "getLength"
    callingConvention :: Text <- runIO $ Java.call dt' "getCallingConventionName" >>= Java.reify
    hasNoReturn :: Bool <- runIO $ Java.call dt' "hasNoReturn"
    returnType :: Maybe DataType <- do
        if hasNoReturn then return Nothing
        else do
            returnType' :: J.DataType <- runIO $ Java.call dt' "getReturnType"
            returnType'' :: DataType <- parseDataType returnType'
            return $ Just returnType''
    return $ FuncDefType FuncDefTypeOpts { width = fromIntegral width
                                         , funcParams = funcParams'
                                         , callingConvention = callingConvention
                                         , returnType = returnType
                                         }


parseDataType' "Undefined" dt = getUndefType dt
parseDataType' "Undefined1DataType" dt = getUndefType dt
parseDataType' "Undefined2DataType" dt = getUndefType dt
parseDataType' "Undefined3DataType" dt = getUndefType dt
parseDataType' "Undefined4DataType" dt = getUndefType dt
parseDataType' "Undefined5DataType" dt = getUndefType dt
parseDataType' "Undefined6DataType" dt = getUndefType dt
parseDataType' "Undefined7DataType" dt = getUndefType dt
parseDataType' "Undefined8DataType" dt = getUndefType dt

parseDataType' _ dt = do
    --width :: Int32 <- runIO $ Java.call dt "getLength" >>= Java.reify
    name :: Text <- runIO $ Java.call dt "getName" >>= Java.reify
    return $ OtherType OtherTypeOpts { name = name }


{- ### Helper Function ### -}

getFuncParamType :: J.ParameterDefinition -> Ghidra FuncParam
getFuncParamType param = do
    dt :: J.DataType <- runIO $ Java.call param "getDataType"
    dataType :: DataType <- parseDataType dt 
    name :: Text <- runIO $ Java.call param "getName" >>= Java.reify
    return $ FuncParam { dataType = dataType, name = name}

getUndefType :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra DataType
getUndefType dt = do
    width :: Int32 <- runIO $ Java.call dt "getLength"
    return $ UndefType UndefTypeOpts { width = fromIntegral width } 

getIntType :: J.DataType -> Ghidra DataType
getIntType dt = do
    let dt' :: J.AbstractIntegerDataType = coerce dt
    signed :: Bool <- runIO $ Java.call dt' "isSigned"
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ IntType IntTypeOpts { width = fromIntegral width, isSigned = signed }

getFloatType :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra DataType
getFloatType dt = do
    width :: Int32 <- runIO $ Java.call dt "getLength"
    return $ FloatType FloatTypeOpts { width = fromIntegral width }

getDataTypeComponentsWithOffsets :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra [(Int32, DataType)]
getDataTypeComponentsWithOffsets dt = do
    components :: [J.DataTypeComponent] <- runIO $ Java.call dt "getDataTypeComponents" >>= Java.reify
    dts :: [J.DataType] <-  mapM (\n -> runIO $ Java.call n "getDataType") components
    offsets :: [Int32] <- mapM (\n -> runIO $ Java.call n "getOffset" >>= Java.reify) components
    datatypes :: [DataType] <- mapM parseDataType dts
    return $ zip offsets datatypes

getDataTypeComponents :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra [DataType]
getDataTypeComponents dt = do
    components :: [J.DataTypeComponent] <- runIO $ Java.call dt "getDataTypeComponents" >>= Java.reify
    dts :: [J.DataType] <-  mapM (\n -> runIO $ Java.call n "getDataType") components
    datatypes :: [DataType] <- mapM parseDataType dts
    return datatypes

getPointerType :: J.DataType -> Ghidra DataType
getPointerType dt = do
    let dt' :: J.PointerDataType = coerce dt
    dt'' :: J.DataType <- runIO $ Java.call dt' "getDataType"
    pointeeType :: DataType <- parseDataType dt'' 
    width :: Int32 <- runIO $ Java.call dt' "getLength"
    return $ PointerType PointerTypeOpts { width = fromIntegral width, pointeeType = pointeeType }
