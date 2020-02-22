{-# LANGUAGE TemplateHaskell #-}
module Blaze.Types.Fir where

import qualified Prelude as P
import           Blaze.Prelude

import Data.Map ((!))
import           Binja.BasicBlock                  ( BasicBlock
                                                   , BasicBlockFunction
                                                   , BlockEdge
                                                   )
import qualified Binja.BasicBlock     as BB
import           Binja.C.Enums                     ( BNBranchType( FalseBranch
                                                                 , TrueBranch
                                                                 )
                                                   )
import           Binja.Core                        ( BNBinaryView
                                                   , InstructionIndex(InstructionIndex)
                                                   
                                                   )
import           Binja.Function                    ( Function
                                                   , MLILSSAFunction
                                                   )
import qualified Binja.Function       as HFunction
import qualified Binja.MLIL           as MLIL
import           Blaze.Function                    ( createCallSite )
import qualified Blaze.Function       as Function
import           Blaze.Graph.Alga                  ( AlgaGraph )
import           Blaze.Types.Function              ( CallInstruction
                                                   , CallSite
                                                   , toCallInstruction
                                                   )
import           Blaze.Types.Graph                 ( Graph )
import qualified Blaze.Types.Graph    as G
import qualified Data.Set as Set
import qualified Streamly.Prelude as S
import Blaze.Types.Path (ConditionNode(ConditionNode))
import qualified Blaze.Graph as Graph
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text


type F = MLILSSAFunction


data IfChainNode n = IfChainNode
  { _commonEscape :: n
  , _destination :: n
  , _self :: n
  , _parent :: n
  } deriving (Eq, Ord, Show, Generic)

instance Hashable n => Hashable (IfChainNode n)

$(makeFieldsNoPrefix ''IfChainNode)

-- newtype ChildSibling n = ChildSibling n
--   deriving (Eq, Ord, Show, Hashable)

data ChainNode n = OriginChainNode n
                 | TraversedChainNode (IfChainNode n)
                 deriving (Eq, Ord, Show, Generic)

instance Hashable n => Hashable (ChainNode n)

-- TODO: needs to keep track of whether True or False branch goes
-- to escape or destination
data IfChain n = IfChain
  { _commonEscape :: n
  , _destination :: n
  , _nodes :: [n]
  } deriving (Eq, Ord, Show, Generic)

$(makeFieldsNoPrefix ''IfChain)

instance Hashable n => Hashable (IfChain n)

instance Pretty (IfChain (BasicBlock F)) where
  pretty (IfChain esc dest ns) =
    "IfChain { esc: " <> showStart esc
    <> ", dest: " <> showStart dest
    <> ", nodes: [" <> Text.intercalate ", " (showStart <$> ns)
    <> "] }"
    where
      showStart bb = show (fromIntegral $ bb ^. BB.start :: Integer)


type ChainMapping n = Map (ChainNode n) (ChainNode n)


data FirNode a = FirBasicBlock a
               | FirIfChain (IfChain a)
               deriving (Eq, Ord, Show)

data FirEdgeLabel e = ChainEscapeEdge
                    | ChainDestinationEdge
                    | NormalEdge e
                    deriving (Eq, Ord, Show)

-- -- eventually should actually contain PIL instead of referring to binja bb
-- type FirBasicBlockNode = BasicBlock F

data FirIfChainNode a = FirIfChainNode
  { _dest :: a
  , _escape :: a
  , _nodes :: a
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''FirIfChainNode)

