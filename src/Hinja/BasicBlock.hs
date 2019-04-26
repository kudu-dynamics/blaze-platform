{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Hinja.BasicBlock where

import Hinja.Prelude hiding (onException, handle)
import qualified Data.Text as Text
import qualified Hinja.C.Main as BN
import Hinja.C.Pointers
import Hinja.Types
import Hinja.Function ( Function
                      , LLILFunction
                      , MLILFunction
                      , MLILSSAFunction
                      , createFunction
                      )
import qualified Hinja.Function as Func
import Hinja.C.Types

data BasicBlock = BasicBlock
  { _handle :: BNBasicBlock
  , _func :: Function
  , _start :: InstructionIndex IL
  , _end :: InstructionIndex IL
  } deriving (Eq, Ord, Show)

data LLILBasicBlock = LLILBasicBlock
  { _handle :: BNBasicBlock
  , _func :: Function
  , _start :: InstructionIndex LLIL
  , _end :: InstructionIndex LLIL
  , _llilFunc :: LLILFunction
  } deriving (Eq, Ord, Show)

data MLILBasicBlock = MLILBasicBlock
  { _handle :: BNBasicBlock
  , _func :: Function
  , _start :: InstructionIndex MLIL
  , _end :: InstructionIndex MLIL
  , _mlilFunc :: MLILFunction
  } deriving (Eq, Ord, Show)

data MLILSSABasicBlock = MLILSSABasicBlock
  { _handle :: BNBasicBlock
  , _func :: Function
  , _start :: InstructionIndex MLILSSA
  , _end :: InstructionIndex MLILSSA
  , _mlilSSAFunc :: MLILSSAFunction
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''BasicBlock)
$(makeFieldsNoPrefix ''LLILBasicBlock)
$(makeFieldsNoPrefix ''MLILBasicBlock)
$(makeFieldsNoPrefix ''MLILSSABasicBlock)



getBasicBlockRange :: BNBasicBlock -> IO (InstructionIndex (), InstructionIndex ())
getBasicBlockRange ptr = (,) <$> BN.getBasicBlockStart ptr <*> BN.getBasicBlockEnd ptr

createBasicBlock :: BNBasicBlock -> IO BasicBlock
createBasicBlock ptr = BasicBlock ptr
  <$> (BN.getBasicBlockFunction ptr >>= createFunction)
  <*> (coerceInstructionIndex <$> BN.getBasicBlockStart ptr)
  <*> (coerceInstructionIndex <$> BN.getBasicBlockEnd ptr)

createLLILBasicBlock :: BNBasicBlock -> IO LLILBasicBlock
createLLILBasicBlock ptr = BN.getBasicBlockFunction ptr >>= createFunction >>= \fn ->
  LLILBasicBlock ptr fn
  <$> (coerceInstructionIndex <$> BN.getBasicBlockStart ptr)
  <*> (coerceInstructionIndex <$> BN.getBasicBlockEnd ptr)
  <*> Func.getLLILFunction fn

createMLILBasicBlock :: BNBasicBlock -> IO MLILBasicBlock
createMLILBasicBlock ptr = BN.getBasicBlockFunction ptr >>= createFunction >>= \fn ->
  MLILBasicBlock ptr fn
  <$> (coerceInstructionIndex <$> BN.getBasicBlockStart ptr)
  <*> (coerceInstructionIndex <$> BN.getBasicBlockEnd ptr)
  <*> Func.getMLILFunction fn

createMLILSSABasicBlock :: BNBasicBlock -> IO MLILSSABasicBlock
createMLILSSABasicBlock ptr = BN.getBasicBlockFunction ptr >>= createFunction >>= \fn ->
  MLILSSABasicBlock ptr fn
  <$> (coerceInstructionIndex <$> BN.getBasicBlockStart ptr)
  <*> (coerceInstructionIndex <$> BN.getBasicBlockEnd ptr)
  <*> Func.getMLILSSAFunction fn

getBasicBlocks :: Function -> IO [BasicBlock]
getBasicBlocks fn = BN.getFunctionBasicBlockList (fn ^. Func.handle)  >>= traverse createBasicBlock

getLLILBasicBlocks :: LLILFunction -> IO [LLILBasicBlock]
getLLILBasicBlocks fn = BN.getLowLevelILBasicBlockList (fn ^. Func.handle)  >>= traverse createLLILBasicBlock

getMLILBasicBlocks :: MLILFunction -> IO [MLILBasicBlock]
getMLILBasicBlocks fn = BN.getMediumLevelILBasicBlockList (fn ^. Func.handle)  >>= traverse createMLILBasicBlock

getMLILSSABasicBlocks :: MLILSSAFunction -> IO [MLILSSABasicBlock]
getMLILSSABasicBlocks fn = BN.getMediumLevelILBasicBlockList (fn ^. Func.handle)  >>= traverse createMLILSSABasicBlock

getBasicBlocksAt :: BNBinaryView -> Address -> IO [BasicBlock]
getBasicBlocksAt bv addr = BN.getBasicBlocksForAddress bv addr
                           >>= traverse createBasicBlock

getLLILBasicBlockForInstruction :: LLILFunction -> InstructionIndex LLIL -> IO (Maybe LLILBasicBlock)
getLLILBasicBlockForInstruction lfn index = 
  BN.getLowLevelILBasicBlockForInstruction (lfn ^. Func.handle) index >>=
  maybe (return Nothing) (fmap Just . createLLILBasicBlock)

getMLILBasicBlockForInstruction :: MLILFunction -> InstructionIndex MLIL -> IO (Maybe MLILBasicBlock)
getMLILBasicBlockForInstruction lfn index = 
  BN.getMediumLevelILBasicBlockForInstruction (lfn ^. Func.handle) index >>=
  maybe (return Nothing) (fmap Just . createMLILBasicBlock)


getMLILSSABasicBlockForInstruction :: MLILSSAFunction -> InstructionIndex MLILSSA -> IO (Maybe MLILSSABasicBlock)
getMLILSSABasicBlockForInstruction lfn index = 
  BN.getMediumLevelILBasicBlockForInstruction (lfn ^. Func.handle) (coerceInstructionIndex index) >>=
  maybe (return Nothing) (fmap Just . createMLILSSABasicBlock)



-- createFunction :: BNFunction -> IO Function
-- createFunction ptr = Function ptr
--                      <$> (Text.pack <$> BN.getFunctionName ptr)
--                      <*> BN.getFunctionStart ptr

-- getFunctions :: BNBinaryView -> IO [Function]
-- getFunctions bv = BN.getFunctions bv >>= traverse createFunction

-- getMLILFunction :: Function -> IO MLILFunction
-- getMLILFunction fn = MLILFunction
--   <$> BN.getFunctionMediumLevelIL (fn ^. handle)
--   <*> pure fn

-- getMLILSSAFunction :: Function -> IO MLILSSAFunction
-- getMLILSSAFunction fn = MLILSSAFunction
--   <$> (BN.getFunctionMediumLevelIL (fn ^. handle)  >>= BN.getMediumLevelILSSAForm)
--   <*> pure fn
