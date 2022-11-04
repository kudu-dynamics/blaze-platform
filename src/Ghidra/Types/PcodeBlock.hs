module Ghidra.Types.PcodeBlock where

import Ghidra.Prelude
import Ghidra.Types.Address (Address)
import qualified Ghidra.Types as J


data PcodeBlock = PcodeBlock
  { handle :: J.PcodeBlockBasic
  , index :: Int32 -- TODO: ensure this is a unique identifier
  } deriving (Show, Generic)

instance Eq PcodeBlock where
  a == b = a ^. #index == b ^. #index

instance Ord PcodeBlock where
  compare a b = compare (a ^. #index) (b ^. #index)

data PcodeBlockGraph a = PcodeBlockGraph
  { nodes :: [a]
  , edges :: [(BranchType, (a, a))]
  } deriving (Eq, Ord, Show, Hashable, Generic, Functor, Foldable, Traversable)

data BranchType
  = UnconditionalBranch -- any time there's only one
  | TrueBranch
  | FalseBranch
  | SwitchBranch
  | UnknownOneOfManyBranch
  deriving (Eq, Ord, Read, Show, Generic, Hashable)

data PcodeBlockType
  = PLAIN -- 0
  | BASIC -- 1
  | GRAPH -- 2
  | COPY -- 3
  | GOTO -- 4
  | MULTIGOTO -- 5
  | LIST -- 6
  | CONDITION -- 7
  | PROPERIF -- 8
  | IFELSE -- 9
  | IFGOTO -- 10
  | WHILEDO -- 11
  | DOWHILE -- 12
  | SWITCH -- 13
  | INFLOOP -- 14
  deriving (Eq, Ord, Read, Show, Generic, Enum)

