{-# LANGUAGE TemplateHaskell #-}

module Hinja.Types.BasicBlock where

import Hinja.Prelude

import Hinja.C.Pointers (BNBasicBlock)
import Hinja.C.Types (InstructionIndex)
import Hinja.C.Enums (BNBranchType)

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
