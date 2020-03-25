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
  } deriving (Ord, Show)

instance Eq fun => Eq (BasicBlock fun) where
  (BasicBlock _ f1 s1 e1) == (BasicBlock _ f2 s2 e2) =
    f1 == f2 && s1 == s2 && e1 == e2

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
