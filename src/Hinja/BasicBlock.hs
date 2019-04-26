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

data BasicBlock fun = BasicBlock
  { _handle :: BNBasicBlock
  , _func :: fun
  , _start :: InstructionIndex fun
  , _end :: InstructionIndex fun
  } deriving (Eq, Ord, Show)

-- data LLILBasicBlock = LLILBasicBlock
--   { _handle :: BNBasicBlock
--   , _func :: Function
--   , _start :: InstructionIndex LLILFunction
--   , _end :: InstructionIndex LLILFunction
--   , _llilFunc :: LLILFunction
--   } deriving (Eq, Ord, Show)

-- data MLILBasicBlock = MLILBasicBlock
--   { _handle :: BNBasicBlock
--   , _func :: Function
--   , _start :: InstructionIndex MLILFunction
--   , _end :: InstructionIndex MLILFunction
--   , _mlilFunc :: MLILFunction
--   } deriving (Eq, Ord, Show)

-- data MLILSSABasicBlock = MLILSSABasicBlock
--   { _handle :: BNBasicBlock
--   , _func :: Function
--   , _start :: InstructionIndex MLILSSAFunction
--   , _end :: InstructionIndex MLILSSAFunction
--   , _mlilSSAFunc :: MLILSSAFunction
--   } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''BasicBlock)
-- $(makeFieldsNoPrefix ''LLILBasicBlock)
-- $(makeFieldsNoPrefix ''MLILBasicBlock)
-- $(makeFieldsNoPrefix ''MLILSSABasicBlock)



getBasicBlockRange :: BNBasicBlock -> IO (InstructionIndex (), InstructionIndex ())
getBasicBlockRange ptr = (,) <$> BN.getBasicBlockStart ptr <*> BN.getBasicBlockEnd ptr

class ConvertBasicBlockFunction a where
  convertBasicBlockFunction :: Function -> IO a

instance ConvertBasicBlockFunction Function where
  convertBasicBlockFunction = return

instance ConvertBasicBlockFunction LLILFunction where
  convertBasicBlockFunction = Func.getLLILFunction

instance ConvertBasicBlockFunction MLILFunction where
  convertBasicBlockFunction = Func.getMLILFunction

instance ConvertBasicBlockFunction MLILSSAFunction where
  convertBasicBlockFunction = Func.getMLILSSAFunction

class GetBlockList a where
  getBlockList :: a -> IO [BNBasicBlock]

instance GetBlockList Function where
  getBlockList = BN.getFunctionBasicBlockList . view Func.handle

instance GetBlockList LLILFunction where
  getBlockList = BN.getLowLevelILBasicBlockList . view Func.handle

instance GetBlockList MLILFunction where
  getBlockList = BN.getMediumLevelILBasicBlockList . view Func.handle

instance GetBlockList MLILSSAFunction where
  getBlockList = BN.getMediumLevelILBasicBlockList . view Func.handle


createBasicBlock :: ConvertBasicBlockFunction t => BNBasicBlock -> IO (BasicBlock t)
createBasicBlock ptr = BasicBlock ptr
  <$> (BN.getBasicBlockFunction ptr >>= createFunction >>= convertBasicBlockFunction)
  <*> (coerceInstructionIndex <$> BN.getBasicBlockStart ptr)
  <*> (coerceInstructionIndex <$> BN.getBasicBlockEnd ptr)


getBasicBlocks :: ( ConvertBasicBlockFunction t
                  , GetBlockList t )
                  => t -> IO [BasicBlock t]
getBasicBlocks fn = getBlockList fn >>= traverse createBasicBlock


getBasicBlocksAt :: BNBinaryView -> Address -> IO [BasicBlock Function]
getBasicBlocksAt bv addr = BN.getBasicBlocksForAddress bv addr
                           >>= traverse createBasicBlock

getLLILBasicBlockForInstruction :: LLILFunction -> InstructionIndex LLILFunction -> IO (Maybe (BasicBlock LLILFunction))
getLLILBasicBlockForInstruction lfn index = 
  BN.getLowLevelILBasicBlockForInstruction (lfn ^. Func.handle) index >>=
  maybe (return Nothing) (fmap Just . createBasicBlock)

getMLILBasicBlockForInstruction :: MLILFunction -> InstructionIndex MLILFunction -> IO (Maybe (BasicBlock MLILFunction))
getMLILBasicBlockForInstruction lfn index = 
  BN.getMediumLevelILBasicBlockForInstruction (lfn ^. Func.handle) index >>=
  maybe (return Nothing) (fmap Just . createBasicBlock)

getMLILSSABasicBlockForInstruction :: MLILSSAFunction -> InstructionIndex MLILSSAFunction -> IO (Maybe (BasicBlock MLILSSAFunction))
getMLILSSABasicBlockForInstruction lfn index = 
  BN.getMediumLevelILBasicBlockForInstruction (lfn ^. Func.handle) (coerceInstructionIndex index) >>=
  maybe (return Nothing) (fmap Just . createBasicBlock)








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
