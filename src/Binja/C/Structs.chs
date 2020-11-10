{-# OPTIONS_GHC -fno-warn-orphans #-}


module Binja.C.Structs where

import Binja.Prelude hiding (join)

import qualified Prelude as P

import Binja.C.Util
import Binja.Types.MLIL
import Binja.Types.Variable ( BNVariable(BNVariable)
                            , BNTypeWithConfidence(BNTypeWithConfidence)
                            , BNBoolWithConfidence(BNBoolWithConfidence)
                            , BNParameterVariablesWithConfidence(BNParameterVariablesWithConfidence))
import qualified Binja.Types.Variable as Variable
import Binja.Types.BasicBlock (BNBasicBlockEdge(BNBasicBlockEdge))
import Binja.Types.StringReference (BNStringReference(BNStringReference))
import qualified Binja.Types.StringReference as StrRef
import qualified Binja.Types.Symbol as Sym
import Binja.Types.Symbol (BNNameSpace(BNNameSpace))
import Binja.Types.TypeLibrary ( BNQualifiedNameAndType(BNQualifiedNameAndType)
                               , BNQualifiedName(BNQualifiedName)
                               , BNFunctionParameter(BNFunctionParameter))

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.String (withCString, CString)
import qualified Data.Text as T
import Foreign.Storable (sizeOf, alignment, peek, poke)

#include <binaryninjacore.h>
  
{#context lib="binaryninjacore" #}

instance Storable MediumLevelILInstruction where
  sizeOf _ = {#sizeof BNMediumLevelILInstruction#}
  alignment _ = {#alignof BNMediumLevelILInstruction#}
  peek p = MediumLevelILInstruction
    <$> liftM (toEnum . fromIntegral) ({#get BNMediumLevelILInstruction->operation #} p)
    <*> liftM mkBool ({#get BNMediumLevelILInstruction->sourceOperand #} p)
    <*> liftM fromIntegral ({#get BNMediumLevelILInstruction->size #} p)
    <*> (OperandsData . fmap fromIntegral <$> (({#get BNMediumLevelILInstruction->operands #} p) >>= peekArray 5))
    <*> liftM fromIntegral ({#get BNMediumLevelILInstruction->address #} p)
    where
      mkBool 1 = True
      mkBool _ = False
  -- only returned, never passed into binja api, so poke not needed
  poke _ _ = P.error "MediumLevelILInstruction 'poke' not implemented"


instance Storable BNVariable where
  sizeOf _ = {#sizeof BNVariable#}
  alignment _ = {#alignof BNVariable#}
  peek p = BNVariable
    <$> liftM (toEnum . fromIntegral) ({#get BNVariable->type #} p)
    <*> liftM fromIntegral ({#get BNVariable->index #} p)
    <*> liftM fromIntegral ({#get BNVariable->storage #} p)
  poke p x = do
     {#set BNVariable->type #} p (fromIntegral . fromEnum $ x ^. Variable.sourceType)
     {#set BNVariable->index #} p (fromIntegral $ x ^. Variable.index)
     {#set BNVariable->storage #} p (fromIntegral $ x ^. Variable.storage)

instance Storable BNTypeWithConfidence where
  sizeOf _ = {#sizeof BNTypeWithConfidence#}
  alignment _ = {#alignof BNTypeWithConfidence#}
  peek p = BNTypeWithConfidence
    <$> ({#get BNTypeWithConfidence->type #} p >>= nilable . castPtr)
    <*> liftM fromIntegral ({#get BNTypeWithConfidence->confidence #} p)
  poke _ _ = P.error "BNTypeWithConfidence 'poke' not implemented"

instance Storable BNBoolWithConfidence where
  sizeOf _ = {#sizeof BNBoolWithConfidence#}
  alignment _ = {#alignof BNBoolWithConfidence#}
  peek p = BNBoolWithConfidence
    <$> ({#get BNBoolWithConfidence->value #} p)
    <*> liftM fromIntegral ({#get BNBoolWithConfidence->confidence #} p)
  poke _ _ = P.error "BNBoolWithConfidence 'poke' not implemented"

instance Storable BNBasicBlockEdge where
  sizeOf _ = {#sizeof BNBasicBlockEdge#}
  alignment _ = {#alignof BNBasicBlockEdge#}
  peek p = BNBasicBlockEdge
    <$> liftM (toEnum . fromIntegral) ({#get BNBasicBlockEdge->type #} p)
    <*> ({#get BNBasicBlockEdge->target #} p >>= nilable_ . castPtr)
    <*> ({#get BNBasicBlockEdge->backEdge #} p)
    <*> ({#get BNBasicBlockEdge->fallThrough #} p)
  poke _ _ = P.error "BNBasicBlockEdge 'poke' not implemented"

instance Storable BNStringReference where
  sizeOf _ = {#sizeof BNStringReference#}
  alignment _ = {#alignof BNStringReference#}
  peek p = BNStringReference
    <$> liftM (toEnum . fromIntegral) ({#get BNStringReference->type #} p)
    <*> liftM fromIntegral ({#get BNStringReference->start #} p)
    <*> liftM fromIntegral ({#get BNStringReference->length #} p)
  poke p x = do
    {#set BNStringReference->type #} p (fromIntegral . fromEnum $ x ^. StrRef.stringType)
    {#set BNStringReference->start #} p (fromIntegral $ x ^. StrRef.start)
    {#set BNStringReference->length #} p (fromIntegral $ x ^. StrRef.length)

instance Storable BNNameSpace where
  sizeOf _ = {#sizeof BNNameSpace#}
  alignment _ = {#alignof BNNameSpace#}
  peek p = do
    count <- fromIntegral <$> ({#get BNNameSpace->nameCount #} p)
    name <- ({#get BNNameSpace->name #} p) >>= peekArray count >>= convertCStrings
    join <- ({#get BNNameSpace->join #} p) >>= peekText
    return $ BNNameSpace name join (fromIntegral count)
      where
        convertCStrings :: [CString] -> IO [Text]
        convertCStrings cStrs = traverse peekText cStrs
  poke p x = do
    withCStringArray (x ^. Sym.name) ({#set BNNameSpace->name #} p)
    withCString (T.unpack (x ^. Sym.join)) ({#set BNNameSpace->join #} p)
    {#set BNNameSpace->nameCount #} p (fromIntegral $ x ^. Sym.nameCount)


instance Storable BNParameterVariablesWithConfidence where
  sizeOf _ = {#sizeof BNParameterVariablesWithConfidence#}
  alignment _ = {#alignof BNParameterVariablesWithConfidence#}
  peek p = do
    count <- fromIntegral <$> ({#get BNParameterVariablesWithConfidence->count #} p)
    vars <- ({#get BNParameterVariablesWithConfidence->vars #} p)
            >>= peekArray count . castPtr
    confidence <- liftM fromIntegral ({#get BNParameterVariablesWithConfidence->confidence #} p)
    return $ BNParameterVariablesWithConfidence vars (fromIntegral count) confidence
  poke _ _ = P.error "BNParameterVariablesWithConfidence 'poke' not implemented"


instance Storable BNQualifiedNameAndType where
  sizeOf _ = {#sizeof BNQualifiedNameAndType#}
  alignment _ = {#alignof BNQualifiedNameAndType#}
  peek p = do
    count <- fromIntegral <$> ({#get BNQualifiedNameAndType->name.nameCount #} p)
    name <- ({#get BNQualifiedNameAndType->name.name #} p)
            >>= peekArray count
            >>= traverse peekText
    join <- ({#get BNQualifiedNameAndType->name.join #} p) >>= peekText
    t <- ({#get BNQualifiedNameAndType->type #} p >>= nilable_ . castPtr)
    let qn = BNQualifiedName name join (fromIntegral count)
    return $ BNQualifiedNameAndType qn t
  poke _ _ = P.error "BNQualifiedNameAndType 'poke' not implemented"

instance Storable BNFunctionParameter where
  sizeOf _ = {#sizeof BNFunctionParameter#}
  alignment _ = {#alignof BNFunctionParameter#}
  peek p = do
    name <- ({#get BNFunctionParameter->name #} p) >>= peekText
    fpType <- ({#get BNFunctionParameter->type #} p >>= nilable_ . castPtr)
    typeConfidence <- liftM fromIntegral ({#get BNFunctionParameter->typeConfidence #} p)
    defaultLocation <- ({#get BNFunctionParameter->defaultLocation #} p)
    sourceType <- liftM (toEnum . fromIntegral) ({#get BNFunctionParameter->location.type #} p)
    idx <- liftM fromIntegral ({#get BNFunctionParameter->location.index #} p)
    storage <- liftM fromIntegral ({#get BNFunctionParameter->location.storage #} p)
    let var' = BNVariable sourceType idx storage
    return $ BNFunctionParameter name fpType typeConfidence defaultLocation var'
  poke _ _ = P.error "BNFunctionParameter 'poke' not implemented"
