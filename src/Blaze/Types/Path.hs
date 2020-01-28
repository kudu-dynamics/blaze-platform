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
import qualified Data.UUID as UUID

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
  } deriving (Eq, Ord, Show, Functor)

mkInsertablePath :: Path p => p -> Maybe (InsertablePath p)
mkInsertablePath p = InsertablePath p <$> firstNode p

data LastIsAbstractCall p = LastIsAbstractCall
  { lacFullPath :: p
  , lacLastNode :: AbstractCallNode
  } deriving (Eq, Ord, Show, Functor)

mkLastIsAbstractCall :: Path p => p -> Maybe (LastIsAbstractCall p)
mkLastIsAbstractCall p = do
  n <- lastNode p
  case n of
    AbstractCall acn -> return $ LastIsAbstractCall p acn
    _ -> Nothing

newtype PathGraph g = PathGraph g
  deriving (Eq, Ord, Show)

deriving instance (Graph () Node g) => Graph () Node (PathGraph g)

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

getNodeFunc :: Node -> Function
getNodeFunc (SubBlock x) = x ^. func
getNodeFunc (Call x) = x ^. func
getNodeFunc (Ret x) = x ^. func
getNodeFunc (AbstractCall x) = x ^. func
getNodeFunc (AbstractPath x) = x ^. func
getNodeFunc (Condition x) = x ^. func


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

callTwaddle :: Word32
callTwaddle = 999

retTwaddle :: Word32
retTwaddle = 500


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
      cnode = Call $ CallNode (acn ^. func) (acn ^. callSite) (twaddleUUID callTwaddle $ acn ^. uuid)
      rnode = Ret $ RetNode (acn ^. func) (acn ^. callSite) (twaddleUUID retTwaddle $ acn ^. uuid)

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
      cnode = Call $ CallNode (acn ^. func) (acn ^. callSite) (twaddleUUID callTwaddle $ acn ^. uuid)
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
startFunction p = do
  n <- firstNode p
  case n of
    SubBlock sb -> return $ sb ^. func
    Call c -> return $ c ^. func
    Ret r -> return $ r ^. func
    AbstractPath apn -> return $ apn ^. func
    _ -> Nothing


