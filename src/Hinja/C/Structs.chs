{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hinja.C.Structs where

import Hinja.Prelude

import qualified Prelude as P
import Foreign.Ptr
import Foreign.Marshal.Array
import Hinja.C.Util
import Hinja.Types.MLIL
import Hinja.Types.Variable
import Hinja.Types.BasicBlock (BNBasicBlockEdge(BNBasicBlockEdge))

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
     {#set BNVariable->index #} p (fromIntegral $ x ^. index)
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
