module Blaze.Import.Source.Ghidra.DataTypeConversions where

import Ghidra.GhidraDataTypes
import Blaze.Prelude hiding (Symbol, isSigned)
import Blaze.Types.Import
import Blaze.Types.Pil.PilType
import Data.Text (append)
import qualified Data.HashMap.Strict as HM

convertGhidraDataTypeToImpType :: GhidraDataType -> ImpType
convertGhidraDataTypeToImpType (FloatType (FloatTypeOpts { width })) = 
    ImpType (TFloat {bitWidth = Just $ expandAndCast width})
convertGhidraDataTypeToImpType (IntType (IntTypeOpts { width, isSigned })) = 
    ImpType (TInt { bitWidth = Just $ expandAndCast width, 
                    signed = Just isSigned})
convertGhidraDataTypeToImpType (StringType (StringTypeOpts { len })) =
    ImpType (TCString { strLen = Just $ fromIntegral len })
convertGhidraDataTypeToImpType (ArrayType (ArrayTypeOpts { elementType, elementWidth, len })) =
    let elemType = convertGhidraDataTypeToImpType elementType
        arrLen = fromIntegral len * elementWidth
    in ImpType (TArray { len = Just $ fromIntegral arrLen, elemType = elemType })
convertGhidraDataTypeToImpType (BadBoyType (BadBoyTypeOpts { width })) = Unknown (Just $ expandAndCast width) "BadBoyType"
convertGhidraDataTypeToImpType (BoolType _) = ImpType TBool
convertGhidraDataTypeToImpType (CharType (CharTypeOpts { width })) =
    ImpType (TChar { bitWidth = Just $ expandAndCast width })

convertGhidraDataTypeToImpType (EnumType (EnumTypeOpts { width, enums=_ })) = Unknown (Just $ expandAndCast width) "EnumType"

convertGhidraDataTypeToImpType (PointerType (PointerTypeOpts { width, pointeeType })) =
    let pointeeType' = convertGhidraDataTypeToImpType pointeeType
    in ImpType (TPointer { bitWidth = Just $ expandAndCast width, pointeeType = pointeeType' })
convertGhidraDataTypeToImpType (StructType (StructTypeOpts { fields })) =
    let offsetMap = convertFieldsToHashMap fields
    in ImpType (TRecord offsetMap)

convertGhidraDataTypeToImpType (UnionType (UnionTypeOpts { width, types=_ })) = Unknown (Just $ expandAndCast width) "UnionType"

convertGhidraDataTypeToImpType (FuncDefType (FuncDefTypeOpts { funcParams, returnType })) =
    let paramTypes = map (convertGhidraDataTypeToImpType . dataType) funcParams
        returnType' = convertGhidraDataTypeToImpType returnType
    in ImpType $ TFunction { ret = returnType', params= paramTypes }

convertGhidraDataTypeToImpType (UndefType (UndefTypeOpts { width })) = Unknown (Just $ expandAndCast width) "UndefType"
convertGhidraDataTypeToImpType (OtherType (OtherTypeOpts { width, name, className })) = 
    let txt = foldl' append "" ["OtherType: ", name, " ", className]
    in Unknown (Just $ expandAndCast width) txt

convertGhidraDataTypeToImpType (VoidType _) = ImpType TUnit -- not sure if this is the best representatin of void
convertGhidraDataTypeToImpType UnknownType = Unknown Nothing "Unknown pointee type"

{- *** Helper Functions *** -}

expandToBits :: (Integral a) => a -> a
expandToBits n = n * 8

expandAndCast :: (Integral a, Num b) => a -> b
expandAndCast = fromIntegral . expandToBits

convertFieldsToHashMap :: [(Int32, GhidraDataType)] -> HashMap BitOffset ImpType
convertFieldsToHashMap = foldl insert' HM.empty
    where
        insert' :: HashMap BitOffset ImpType -> (Int32, GhidraDataType) -> HashMap BitOffset ImpType
        insert' hMap (offset, dataType) =
            let dt = convertGhidraDataTypeToImpType dataType
            in HM.insert (expandAndCast offset) dt hMap