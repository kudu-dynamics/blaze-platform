{-# LANGUAGE TemplateHaskell #-}
module Blaze.Types.Fir where

import           Blaze.Prelude

import Binja.BasicBlock as BB
import Binja.Function (MLILSSAFunction)

import           Blaze.Pretty

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
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

$(makeFieldsNoPrefix ''IfChain)

instance Hashable n => Hashable (IfChain n)

instance Tokenizable (IfChain (BasicBlock F)) where
  tokenize (IfChain esc dest ns) =
    [kt "IfChain", tt " { "] <++>
    arg "esc" (tokStart esc) True <++>
    arg "dest" (tokStart dest) True <++>
    arg "nodes" (delimitedList [tt "["] [tt ", "] [tt "]"] $ tokStart <$> ns) False <++>
    tt " }"
    where
      tokStart bb = [tt $ show (fromIntegral $ bb ^. BB.start :: Integer)]
      arg name val more = [plainToken ArgumentNameToken name, tt ": "] <++> val <++> [tt ", " | more]


type ChainMapping n = Map (ChainNode n) (ChainNode n)


data FirNode a = FirBasicBlock a
               | FirIfChain (IfChain a)
               deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable)

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
