{-# LANGUAGE TemplateHaskell #-}

module Binja.Types.BasicBlock where

import Binja.Prelude

import Binja.C.Pointers (BNBasicBlock)
import Binja.C.Types (InstructionIndex)
import Binja.C.Enums (BNBranchType)

data BasicBlock fun = BasicBlock
  { _handle :: BNBasicBlock
  , _func :: fun
  , _start :: InstructionIndex fun
  , _end :: InstructionIndex fun
  } deriving (Eq, Ord, Show)


data BlockEdge fun = BlockEdge
  { _src :: BasicBlock fun
  , _target :: Maybe (BasicBlock fun)
  , _branchType :: BNBranchType
  , _isBackEdge :: Bool
  , _isFallThrough :: Bool
  } deriving (Eq, Ord, Show)


data BNBasicBlockEdge = BNBasicBlockEdge
  { _branchType :: BNBranchType
  , _target :: Maybe BNBasicBlock
  , _isBackEdge :: Bool
  , _isFallThrough :: Bool
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''BasicBlock)
$(makeFieldsNoPrefix ''BlockEdge)
$(makeFieldsNoPrefix ''BNBasicBlockEdge)
