module Blaze.Types.Fir where

import           Blaze.Prelude

import Binja.BasicBlock as BB
import Binja.Function (MLILSSAFunction)

import           Blaze.Pretty

type F = MLILSSAFunction

data IfChainNode n = IfChainNode
  { commonEscape :: n
  , destination :: n
  , self :: n
  , parent :: n
  } deriving (Eq, Ord, Show, Generic)

instance Hashable n => Hashable (IfChainNode n)

-- newtype ChildSibling n = ChildSibling n
--   deriving (Eq, Ord, Show, Hashable)

data ChainNode n = OriginChainNode n
                 | TraversedChainNode (IfChainNode n)
                 deriving (Eq, Ord, Show, Generic)

instance Hashable n => Hashable (ChainNode n)

-- TODO: needs to keep track of whether True or False branch goes
-- to escape or destination
data IfChain n = IfChain
  { commonEscape :: n
  , destination :: n
  , nodes :: [n]
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

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

data FirEdgeLabel l = ChainEscapeEdge
                    | ChainDestinationEdge
                    | Standard l
                    deriving (Eq, Ord, Show)

-- -- eventually should actually contain PIL instead of referring to binja bb
-- type FirBasicBlockNode = BasicBlock F

data FirIfChainNode a = FirIfChainNode
  { dest :: a
  , escape :: a
  , nodes :: a
  } deriving (Eq, Ord, Show, Generic)
