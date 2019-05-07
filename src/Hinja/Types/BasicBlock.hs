{-# LANGUAGE TemplateHaskell #-}

module Hinja.Types.BasicBlock where

import Hinja.Prelude

import Hinja.C.Pointers (BNBasicBlock)
import Hinja.C.Types (InstructionIndex)

data BasicBlock fun = BasicBlock
  { _handle :: BNBasicBlock
  , _func :: fun
  , _start :: InstructionIndex fun
  , _end :: InstructionIndex fun
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''BasicBlock)
