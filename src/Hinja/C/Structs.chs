{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Hinja.C.Structs where

import Hinja.Prelude

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Data.Vector.Fixed.Storable (Vec5)

#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#include "/tmp/beauty/binaryninjacore.h"

{#context lib="binaryninjacore" #}

-- billy :: Ptr () -> IO CUInt
-- billy = {#get BNMediumLevelILInstruction->sourceOperand #}

-- billy2 :: Ptr () -> IO [CULong]
-- billy2 ptr = ({#get BNMediumLevelILInstruction->operands #} ptr) >>= peekArray 5

-- billy3 :: Ptr CULong -> Ptr CULong -> IO ()
-- billy3 = {#set BNMediumLevelILInstruction->operands #}

-- data MediumLevelILInstruction = MediumLevelILInstruction
--   { _operation :: CUInt
--   , _sourceOperand :: CUInt
--   , _size :: CSize
--   , _operands :: [CULong]
--   , _address :: CULong
--   } deriving (Eq, Ord, Show)

-- instance Storable MediumLevelILInstruction where
--   sizeOf _ = {#sizeof BNMediumLevelILInstruction#}
--   alignment _ = {#alignof BNMediumLevelILInstruction#}
--   peek p = MediumLevelILInstruction
--     <$> liftM fromIntegral ({#get BNMediumLevelILInstruction->operation #} p)
--     <*> liftM fromIntegral ({#get BNMediumLevelILInstruction->sourceOperand #} p)
--     <*> liftM fromIntegral ({#get BNMediumLevelILInstruction->size #} p)
--     <*> (({#get BNMediumLevelILInstruction->operands #} p) >>= peekArray 5)
--     <*> liftM fromIntegral ({#get BNMediumLevelILInstruction->address #} p)
--   poke p x = do
--     {#set BNMediumLevelILInstruction->operation #} p (fromIntegral $  _operation x)
--     {#set BNMediumLevelILInstruction->sourceOperand #} p (fromIntegral $  _sourceOperand x)
--     {#set BNMediumLevelILInstruction->size #} p (fromIntegral $  _size x)
-- --    (_operands x) >>= {#set BNMediumLevelILInstruction->operands #} p 
