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

data BasicBlock fun = BasicBlock
  { _handle :: BNBasicBlock
  , _func :: fun
  , _start :: InstructionIndex fun
  , _end :: InstructionIndex fun
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''BasicBlock)
-- $(makeFieldsNoPrefix ''LLILBasicBlock)
-- $(makeFieldsNoPrefix ''MLILBasicBlock)
-- $(makeFieldsNoPrefix ''MLILSSABasicBlock)

getBasicBlockRange :: BNBasicBlock -> IO (InstructionIndex (), InstructionIndex ())
getBasicBlockRange ptr = (,) <$> BN.getBasicBlockStart ptr <*> BN.getBasicBlockEnd ptr

class BasicBlockFunction fun where
  convertToBasicBlockFunction :: Function -> IO fun
  getBlockPtrList :: fun -> IO [BNBasicBlock]

instance BasicBlockFunction Function where
  convertToBasicBlockFunction = return
  getBlockPtrList = BN.getFunctionBasicBlockList . view Func.handle

instance BasicBlockFunction LLILFunction where
  convertToBasicBlockFunction = Func.getLLILFunction
  getBlockPtrList = BN.getLowLevelILBasicBlockList . view Func.handle

instance BasicBlockFunction MLILFunction where
  convertToBasicBlockFunction = Func.getMLILFunction
  getBlockPtrList = BN.getMediumLevelILBasicBlockList . view Func.handle

instance BasicBlockFunction MLILSSAFunction where
  convertToBasicBlockFunction = Func.getMLILSSAFunction
  getBlockPtrList = BN.getMediumLevelILBasicBlockList . view Func.handle

class BasicBlockInstructionFunction fun where
  getBasicBlockPtrForInstruction :: fun -> InstructionIndex fun -> IO (Maybe BNBasicBlock)

instance BasicBlockInstructionFunction LLILFunction where
  getBasicBlockPtrForInstruction fn index =
    BN.getLowLevelILBasicBlockForInstruction (fn ^. Func.handle) index

instance BasicBlockInstructionFunction MLILFunction where
  getBasicBlockPtrForInstruction fn index =
    BN.getMediumLevelILBasicBlockForInstruction (fn ^. Func.handle) index

instance BasicBlockInstructionFunction MLILSSAFunction where
  getBasicBlockPtrForInstruction fn index =
    BN.getMediumLevelILBasicBlockForInstruction (fn ^. Func.handle) (coerceInstructionIndex index)


createBasicBlock :: BasicBlockFunction t => BNBasicBlock -> IO (BasicBlock t)
createBasicBlock ptr = BasicBlock ptr
  <$> (BN.getBasicBlockFunction ptr >>= createFunction >>= convertToBasicBlockFunction)
  <*> (coerceInstructionIndex <$> BN.getBasicBlockStart ptr)
  <*> (coerceInstructionIndex <$> BN.getBasicBlockEnd ptr)

getBasicBlocks :: BasicBlockFunction t => t -> IO [BasicBlock t]
getBasicBlocks fn = getBlockPtrList fn >>= traverse createBasicBlock

getBasicBlocksAtAddress :: BNBinaryView -> Address -> IO [BasicBlock Function]
getBasicBlocksAtAddress bv addr = BN.getBasicBlocksForAddress bv addr
                                  >>= traverse createBasicBlock

getBasicBlockForInstruction :: (BasicBlockFunction fun, BasicBlockInstructionFunction fun)
                            => fun -> InstructionIndex fun -> IO (Maybe (BasicBlock fun))
getBasicBlockForInstruction fn index = getBasicBlockPtrForInstruction fn index >>=
  maybe (return Nothing) (fmap Just . createBasicBlock)
