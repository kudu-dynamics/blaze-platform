{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Path where

import Binja.Core (InstructionIndex)
import Binja.Function (Function)
import Blaze.Types.Function (CallSite)
import Blaze.Types.Graph (Graph)
import qualified Binja.MLIL as MLIL
import qualified Blaze.Types.Graph as G
import qualified Data.Set as Set
import qualified Prelude as P
import Blaze.Prelude hiding (succ, pred, toList)
import Binja.Function (MLILSSAFunction)

type F = MLILSSAFunction

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

instance (Graph () Node g) => Path (PathGraph g) where
  toList g = case firstNode g of
    Nothing -> []
    Just x -> x:(getRest $ succ x g)
      where
        getRest Nothing = []
        getRest (Just y) = y : getRest (succ y g)

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

  expandNode apn gpart g = maybe g identity $ do
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
          | AbstractCall AbstractCallNode
          | AbstractPath AbstractPathNode
          | Condition ConditionNode
          deriving (Eq, Ord, Show)

data ConditionNode = ConditionNode
  { _func :: Function
  , _trueOrFalseBranch :: Bool
  , _condition :: MLIL.Expression F
  , _uuid :: UUID
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

data AbstractCallNode = AbstractCallNode
  { _func :: Function
  , _callSite :: CallSite
  , _uuid :: UUID
  } deriving (Eq, Ord, Show)


$(makeFieldsNoPrefix ''SubBlockNode)
$(makeFieldsNoPrefix ''ConditionNode)
$(makeFieldsNoPrefix ''CallNode)
$(makeFieldsNoPrefix ''RetNode)
$(makeFieldsNoPrefix ''AbstractPathNode)
$(makeFieldsNoPrefix ''AbstractCallNode)


startFunction :: Path p => p -> Maybe Function
startFunction p = do
  n <- firstNode p
  case n of
    SubBlock sb -> return $ sb ^. func
    Call c -> return $ c ^. func
    Ret r -> return $ r ^. func
    AbstractPath apn -> return $ apn ^. func
    _ -> Nothing
