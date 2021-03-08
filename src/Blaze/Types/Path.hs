{-# LANGUAGE UndecidableInstances #-}

module Blaze.Types.Path where

import Binja.Core (InstructionIndex)
import Binja.Function (Function, MLILSSAFunction)
import qualified Binja.MLIL as MLIL

import Blaze.Import.Source.BinaryNinja.Types (CallSite)
import Blaze.Types.Graph (Graph)
import qualified Blaze.Types.Graph as G

import qualified Data.Set as Set
import qualified Prelude as P
import Blaze.Prelude hiding (succ, pred, toList)

type F = MLILSSAFunction

class Path p where
  fromList :: [Node] -> p
  toList :: p -> [Node]
  succ :: Node -> p -> Maybe Node
  pred :: Node -> p -> Maybe Node
  firstNode :: p -> Maybe Node
  lastNode :: p -> Maybe Node

  -- if AbstractCallNode isn't in second p, just returns p
  expandAbstractCall :: AbstractCallNode -> InsertablePath p -> p -> p

  -- expands second path without Ret node
  expandLast :: InsertablePath p -> LastIsAbstractCall p -> p

  contains :: Node -> p -> Bool

  -- -- removes all nodes after node.
  -- expandAbstractCallSnipAfter :: AbstractCallNode -> p -> p -> p

data InsertablePath p = InsertablePath
  { insertableFullPath :: p
  , insertableFirstNode :: Node -- proof that it's not empty
  } deriving (Eq, Ord, Show, Functor, Generic)

mkInsertablePath :: Path p => p -> Maybe (InsertablePath p)
mkInsertablePath p = InsertablePath p <$> firstNode p

data LastIsAbstractCall p = LastIsAbstractCall
  { lacFullPath :: p
  , lacLastNode :: AbstractCallNode
  } deriving (Eq, Ord, Show, Functor, Generic)

mkLastIsAbstractCall :: Path p => p -> Maybe (LastIsAbstractCall p)
mkLastIsAbstractCall p = do
  n <- lastNode p
  case n of
    AbstractCall acn -> return $ LastIsAbstractCall p acn
    _ -> Nothing

newtype PathGraph g = PathGraph g
  deriving (Eq, Ord, Show, Generic)

deriving newtype instance (Graph () () Node g) => Graph () () Node (PathGraph g)

-- expandAbstractCall :: Path p => AbstractCallNode -> p -> p -> p
-- expandAbstractCall = expandAbstractCall_ True

-- expandAbstractCallWithoutRet :: Path p => AbstractCallNode -> p -> p -> p
-- expandAbstractCallWithoutRet = expandAbstractCall_ False

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
          deriving (Eq, Ord, Show, Generic)


data ConditionNode = ConditionNode
  { func :: Function
  , trueOrFalseBranch :: Bool
  , condition :: MLIL.Expression F
  , uuid :: UUID
  } deriving (Eq, Ord, Show, Generic)

data SubBlockNode = SubBlockNode
  { func :: Function
  , blockStart :: InstructionIndex F
  , start :: InstructionIndex F
  , end :: InstructionIndex F
  , uuid :: UUID
  } deriving (Eq, Ord, Show, Generic)

data CallNode = CallNode
  { func :: Function
  , callSite :: CallSite
  , uuid :: UUID
  } deriving (Eq, Ord, Show, Generic)

data RetNode = RetNode
  { func :: Function
  , callSite :: CallSite
  , uuid :: UUID
  } deriving (Eq, Ord, Show, Generic)

-- we are unsure if this will be useful
data AbstractPathNode = AbstractPathNode
  { func :: Function
  , startNode :: Node
  , endNode :: Node
  , uuid :: UUID
  } deriving (Eq, Ord, Show, Generic)

-- use this instead of the old CallNode / AbstractPathNode / RetNode combo
data AbstractCallNode = AbstractCallNode
  { func :: Function
  , callSite :: CallSite
  , uuid :: UUID
  } deriving (Eq, Ord, Show, Generic)

getNodeFunc :: Node -> Function
getNodeFunc (SubBlock x) = x ^. #func
getNodeFunc (Call x) = x ^. #func
getNodeFunc (Ret x) = x ^. #func
getNodeFunc (AbstractCall x) = x ^. #func
getNodeFunc (AbstractPath x) = x ^. #func
getNodeFunc (Condition x) = x ^. #func

callTwaddle :: Word32
callTwaddle = 999

retTwaddle :: Word32
retTwaddle = 500

instance (Graph () () Node g) => Path (PathGraph g) where
  toList g = case firstNode g of
    Nothing -> []
    Just x -> x : getRest (succ x g)
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

  -- TODO: VERY inefficent! change datatype to include first and last node
  firstNode g = case Set.toList $ G.sources g of
    [] -> Nothing
    [x] -> Just x
    _ -> P.error "Path has multiple source nodes. Bad!"

  -- TODO: VERY inefficent! change datatype to include first and last node
  lastNode g = case Set.toList $ G.sinks g of
    [] -> Nothing
    [x] -> Just x
    _ -> P.error "Path has multiple sink nodes. Bad!"

  -- this probably shouldn't just return 'p' if it fails..
  expandAbstractCall acn ip p =
    flip G.addEdges (G.removeNode n p)
    . fmap ((),) . (<> ppartEdges) . concat
    $ [ maybe [] ((:[]) . (, cnode)) mpred
      , [ (cnode, firstN) ]
      , concat [ [(lastN, rnode)]
               , maybe [] ((:[]) . (rnode,)) msucc
               ]
      ]
    where
      mpred = pred n p
      msucc = succ n p

      -- just twaddling the AbstractPathNode's UUID since we don't have IO.
      cnode = Call $ CallNode (acn ^. #func) (acn ^. #callSite) (twaddleUUID callTwaddle $ acn ^. #uuid)
      rnode = Ret $ RetNode (acn ^. #func) (acn ^. #callSite) (twaddleUUID retTwaddle $ acn ^. #uuid)

      ppart = insertableFullPath ip
      firstN = insertableFirstNode ip
      -- lastNode will never return Nothing b/c InsertablePath is not empty
      lastN = fromJust $ lastNode ppart
      ppartList = toList ppart
      ppartEdges = zip ppartList (drop 1 ppartList)
      n = AbstractCall acn

  expandLast ip lac =
    flip G.addEdges (G.removeNode n p)
    . fmap ((),) . (<> ppartEdges) . concat
    $ [ maybe [] ((:[]) . (, cnode)) mpred
      , [ (cnode, firstN) ]
      ]
    where
      cnode = Call $ CallNode (acn ^. #func) (acn ^. #callSite) (twaddleUUID callTwaddle $ acn ^. #uuid)
      firstN = insertableFirstNode ip
      acn = lacLastNode lac
      n = AbstractCall acn
      mpred = pred n $ lacFullPath lac
      p = lacFullPath lac
      ppart = insertableFullPath ip
      ppartList = toList ppart
      ppartEdges = zip ppartList (drop 1 ppartList)
      
  contains = G.hasNode

  -- snipAfter n p = maybe G.empty identity $ do

startFunction :: Path p => p -> Maybe Function
startFunction p = getNodeFunc <$> firstNode p
