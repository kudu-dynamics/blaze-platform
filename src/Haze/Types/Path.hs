{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Haze.Types.Path where

import Hinja.Core (InstructionIndex)
import Hinja.Function (Function)
import Haze.Types.Function (CallSite)
import Haze.Types.Graph (Graph)
import qualified Hinja.MLIL as MLIL
import qualified Haze.Types.Graph as G
import qualified Data.Set as Set
import qualified Prelude as P

import Haze.Prelude hiding (succ, pred, toList)

class Path p where
  fromList :: [Node] -> p
  toList :: p -> [Node]
  succ :: Node -> p -> Maybe Node
  pred :: Node -> p -> Maybe Node
  firstNode :: p -> Maybe Node
  lastNode :: p -> Maybe Node
  expandNode :: AbstractPathNode -> p -> p -> p

newtype PathGraph g = PathGraph g

deriving instance (Graph () Node g) => Graph () Node (PathGraph g)

repeatOnPreviousUntilEmpty :: (a -> [a]) -> a -> [a]
repeatOnPreviousUntilEmpty f a = case f a of
  [] -> []
  [x] -> x : repeatOnPreviousUntilEmpty f x
  (x:_) -> x : repeatOnPreviousUntilEmpty f x -- sad! shouldn't happen

instance (Graph () Node g) => Path (PathGraph g) where
  toList g = case Set.toList $ G.sources g of
    [] -> []
    [x] -> repeatOnPreviousUntilEmpty (Set.toList . flip G.succs g) x
    _ -> P.error "Path node has multiple sources. Bad!"

  fromList [] = G.empty
  fromList [a] = G.fromNode a
  fromList (x:xs) = G.fromEdges . fmap ((),) $ zip (x:xs) xs

  succ node g = case Set.toList $ G.succs node g of
    [] -> Nothing
    [x] -> Just x
    _ -> P.error "Path node has multiple successors. Bad!"

  pred node g = case Set.toList $ G.preds node g of
    [] -> Nothing
    [x] -> Just x
    _ -> P.error "Path node has multiple predecessors. Bad!"

  firstNode g = case Set.toList $ G.sources g of
    [] -> Nothing
    [x] -> Just x
    _ -> P.error "Path has multiple source nodes. Bad!"

  lastNode g = case Set.toList $ G.sinks g of
    [] -> Nothing
    [x] -> Just x
    _ -> P.error "Path has multiple sink nodes. Bad!"

  expandNode apn gpart g = maybe g id $ do
    firstN <- firstNode gpart
    lastN <- lastNode gpart
    npred <- pred n g
    nsucc <- succ n g
    let g' = G.removeEdges [(npred, n), (n, nsucc)] g
        g'' = flip G.addEdges g'
              . fmap ((),) $ [(npred, firstN), (lastN, nsucc)] <> gpartEdges
    return g''
    where
      gpartList = toList gpart
      gpartEdges = zip gpartList (drop 1 gpartList)
      n = AbstractPath apn

-- instance Graph edge node g => Path (PathGraph g) where
--   fromList :: (Graph () Node g) => [Node] -> PathGraph g
--   fromList [] = G.empty
--   fromList [a] = G.fromNode a
--   fromList (x:xs) = G.fromEdges . fmap ((),) $ zip (x:xs) xs


data Node = SubBlock SubBlockNode
          | Call CallNode
          | Ret RetNode
          | AbstractPath AbstractPathNode
          | Condition ConditionNode
          | Bdup
          deriving (Eq, Ord, Show)

newtype ConditionNode = ConditionNode
  { _condition :: MLIL.Operation (MLIL.Expression F)
  } deriving (Eq, Ord, Show)

data SubBlockNode = SubBlockNode
  { _func :: Function
  , _blockStart :: InstructionIndex F
  , _start :: InstructionIndex F
  , _end :: InstructionIndex F
  , _uuid :: UUID
  } deriving (Eq, Ord, Show)

data CallNode = CallNode
  { _func :: Function
  , _callSite :: CallSite
  , _uuid :: UUID
  } deriving (Eq, Ord, Show)

data RetNode = RetNode
  { _func :: Function
  , _callSite :: CallSite
  , _uuid :: UUID
  } deriving (Eq, Ord, Show)

data AbstractPathNode = AbstractPathNode
  { _func :: Function
  , _startNode :: Node
  , _endNode :: Node
  , _uuid :: UUID
  } deriving (Eq, Ord, Show)


$(makeFieldsNoPrefix ''SubBlockNode)
$(makeFieldsNoPrefix ''CallNode)
$(makeFieldsNoPrefix ''RetNode)
$(makeFieldsNoPrefix ''AbstractPathNode)

