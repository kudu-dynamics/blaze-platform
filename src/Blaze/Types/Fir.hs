{-# LANGUAGE TemplateHaskell #-}
module Blaze.Types.Fir where

import           Blaze.Prelude

import Binja.BasicBlock as BB
import Binja.Function (MLILSSAFunction)

import           Blaze.Pretty

import qualified Data.Text            as Text

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
