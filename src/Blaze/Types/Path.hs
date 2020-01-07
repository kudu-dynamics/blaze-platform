{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Path where

import Binja.Core (InstructionIndex)
import Blaze.Types.Function (CallSite)
import qualified Blaze.Types.Function as BFunc
import Blaze.Types.Graph (Graph)
import qualified Binja.MLIL as MLIL
import qualified Blaze.Types.Graph as G
import qualified Data.Set as Set
import qualified Prelude as P
import Blaze.Prelude hiding (succ, pred, toList)
import Binja.Function (Function, MLILSSAFunction)

type F = MLILSSAFunction

class Path p where
  fromList :: [Node] -> p
  toList :: p -> [Node]
  succ :: Node -> p -> Maybe Node
  pred :: Node -> p -> Maybe Node
  firstNode :: p -> Maybe Node
  lastNode :: p -> Maybe Node
  expandNode :: AbstractPathNode -> p -> p -> p

  -- first p is path to replace AbstractPathNode
  expandAbstractCall :: AbstractCallNode -> p -> p -> p
  
  contains :: Node -> p -> Bool

  -- -- removes all nodes after node.
  -- expandAbstractCallSnipAfter :: AbstractCallNode -> p -> p -> p

newtype PathGraph g = PathGraph g
  deriving (Eq, Ord, Show)

deriving instance (Graph () Node g) => Graph () Node (PathGraph g)


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

-- we are unsure if this will be useful
data AbstractPathNode = AbstractPathNode
  { _func :: Function
  , _startNode :: Node
  , _endNode :: Node
  , _uuid :: UUID
  } deriving (Eq, Ord, Show)

-- use this instead of the old CallNode / AbstractPathNode / RetNode combo
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

paren :: Text -> Text
paren t = "(" <> t <> ")"

brack :: Text -> Text
brack t = "[" <> t <> "]"

quote :: Text -> Text
quote t = "\"" <> t <> "\""

quote' :: Text -> Text
quote' t = "'" <> t <> "'"

instance Pretty Node where
  pretty (SubBlock x) =
    brack (pretty (x ^. start) <> "-" <> pretty (x ^. end - 1)) <> " : SubBlock"
  pretty (Call x) =
    "-------Expanding call: " <> pretty (x ^. callSite)
  pretty (Ret x) =
    "-------Returning to " <> pretty (x ^. func) <> " from " <> pretty (x ^. callSite . BFunc.callDest)
  pretty (AbstractCall x) =
    brack (pretty $ x ^. callSite . BFunc.callInstr . BFunc.index)
    <> " : "
    <> pretty (x ^. callSite)
  pretty (AbstractPath _) = "AbstractPath"
  pretty (Condition x) =
    "Condition: " <> (bool "NOT " "" $ x ^. trueOrFalseBranch)
    <> pretty (x ^. condition)

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

  expandAbstractCall acn ppart p = maybe p identity $ do
    firstN <- firstNode ppart
    lastN <- lastNode ppart
    let mpred = pred n p
        msucc = succ n p
    -- npred <- pred n p
    -- nsucc <- succ n p

    --- reusing the AbstractCallNode's uuid because we're not in IO
    --  and the uuid is just there to make sure nodes of the same type
    --  are distinguishable.
    let cnode = Call $ CallNode (acn ^. func) (acn ^. callSite) (acn ^. uuid)
        rnode = Ret $ RetNode (acn ^. func) (acn ^. callSite) (acn ^. uuid)
    let p' = G.removeNode n p
        p'' = flip G.addEdges p'
              . fmap ((),) . (<> ppartEdges) . concat
              $ [ maybe [] ((:[]) . (, cnode)) mpred
                , [ (cnode, firstN)
                  , (lastN, rnode)
                  ]
                , maybe [] ((:[]) . (rnode,)) msucc
                ] 
    return p''
    where
      ppartList = toList ppart
      ppartEdges = zip ppartList (drop 1 ppartList)
      n = AbstractCall acn

  contains = G.hasNode

  -- snipAfter n p = maybe G.empty identity $ do
    
    

startFunction :: Path p => p -> Maybe Function
startFunction p = do
  n <- firstNode p
  case n of
    SubBlock sb -> return $ sb ^. func
    Call c -> return $ c ^. func
    Ret r -> return $ r ^. func
    AbstractPath apn -> return $ apn ^. func
    _ -> Nothing


