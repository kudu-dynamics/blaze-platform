{-# LANGUAGE TemplateHaskell #-}

module Haze.Types.Path where

import Hinja.Core (InstructionIndex)
import Hinja.Function (Function)
import Haze.Types.Function (CallSite)

import Haze.Prelude

-- data Path = Path { _graph :: [Node] }

data Node = SubBlock SubBlockNode
          | Call CallNode
          | Ret RetNode
          | AbstractPath AbstractPathNode
          deriving (Eq, Ord, Show)

data SubBlockNode = SubBlockNode
  { _func :: Function
  , _blockStart :: InstructionIndex F
  , _start :: InstructionIndex F
  , _end :: InstructionIndex F
  } deriving (Eq, Ord, Show)

data CallNode = CallNode
  { _func :: Function
  , _callSite :: CallSite
  } deriving (Eq, Ord, Show)

data RetNode = RetNode
  { _func :: Function
  , _callSite :: CallSite
  } deriving (Eq, Ord, Show)

data AbstractPathNode = AbstractPathNode
  { _func :: Function
  , _startNode :: Node
  , _endNode :: Node
  } deriving (Eq, Ord, Show)


$(makeFieldsNoPrefix ''SubBlockNode)
$(makeFieldsNoPrefix ''CallNode)
$(makeFieldsNoPrefix ''RetNode)
$(makeFieldsNoPrefix ''AbstractPathNode)

