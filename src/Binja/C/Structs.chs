{-# OPTIONS_GHC -fno-warn-orphans #-}

module Binja.C.Structs where

import Binja.Prelude

import qualified Prelude as P
import Foreign.Ptr
import Foreign.Marshal.Array
import Binja.C.Util
import Binja.Types.MLIL
import Binja.Types.Variable
import qualified Binja.Types.Variable as Variable
import Binja.Types.BasicBlock (BNBasicBlockEdge(BNBasicBlockEdge))
import Binja.Types.StringReference (BNStringReference(BNStringReference))
import qualified Binja.Types.StringReference as StrRef

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
     {#set BNVariable->type #} p (fromIntegral . fromEnum $ x ^. sourceType)
     {#set BNVariable->index #} p (fromIntegral $ x ^. Variable.index)
     {#set BNVariable->storage #} p (fromIntegral $ x ^. storage)

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