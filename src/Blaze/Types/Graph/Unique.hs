module Blaze.Types.Graph.Unique where

-- A unique graph implementation
import Blaze.Prelude
import qualified Blaze.Types.Graph as G
import Blaze.Types.Graph (Graph)
import Blaze.Types.Graph.Alga (AlgaGraph)
import System.Random (Random)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HMap

newtype NodeId = NodeId UUID
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Random)
  deriving anyclass (Hashable, FromJSON, ToJSON)

genNodeId :: MonadIO m => m NodeId
genNodeId = NodeId <$> liftIO randomIO

newtype BuilderState n = BuilderState
  { nodeToIdMap :: HashMap n NodeId
  } deriving (Eq, Ord, Show, Generic)

emptyBuilderState :: BuilderState n
emptyBuilderState = BuilderState $ HMap.empty

newtype Builder n m a = Builder
  { _runBuilder :: StateT (BuilderState n) m a }
  deriving (Functor, Generic)
  deriving newtype ( Applicative
                   , Monad
                   , MonadIO
                   , MonadState (BuilderState n)
                   )

build' :: BuilderState n -> Builder n m a -> m (a, BuilderState n)
build' s m = runStateT (_runBuilder m) s

build :: Functor m => Builder n m a -> m a
build = fmap fst . build' emptyBuilderState

-- | Looks up node to see if it's already been added, or generates new id
-- TODO: Change MonadIO constraint to some MTL uuid gen class
add :: (Eq n, Hashable n, MonadIO m) => n -> Builder n m (Unique n)
add n = do
  m <- use #nodeToIdMap
  case HMap.lookup n m of
    Nothing -> do
      nid <- genNodeId
      #nodeToIdMap %= HMap.insert n nid
      return $ Unique nid n
    Just nid -> return $ Unique nid n
      

data Unique n = Unique
  { nodeId :: NodeId
  , node :: n
  } deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, FromJSON)

-- TODO: change this to some MTL class for just doing UUIDs
mkUnique :: MonadIO m => n -> m (Unique n)
mkUnique n = flip Unique n  <$> liftIO randomIO

newtype UniqueGraph e n = UniqueGraph (AlgaGraph e n NodeId)
  deriving (Generic, Show, Ord, Eq)

-- | sets unique attr for existing NodeId
setUniqueAttr :: Unique n -> UniqueGraph e n -> UniqueGraph e n
setUniqueAttr n (UniqueGraph g) = UniqueGraph $ G.setNodeAttr (n ^. #node) (n ^. #nodeId) g

setUniqueAttrs :: Foldable t
  => t (Unique n)
  -> UniqueGraph e n
  -> UniqueGraph e n
setUniqueAttrs xs g = foldr setUniqueAttr g xs

getUnique :: AlgaGraph e n NodeId -> NodeId -> Unique n
getUnique g nid = Unique nid . fromJust $ G.getNodeAttr nid g

getUniqueSet :: Ord n => AlgaGraph e n NodeId -> Set NodeId -> Set (Unique n)
getUniqueSet g = Set.fromList . fmap (getUnique g) . Set.toList

unUnique :: Functor f => f (Unique n) -> f NodeId
unUnique = fmap $ view #nodeId

instance Ord n => Graph e () (Unique n) (UniqueGraph e n) where
  empty = UniqueGraph G.empty
  fromNode n = UniqueGraph
    . G.setNodeAttr (n ^. #node) (n ^. #nodeId)
    $ G.fromNode (n ^. #nodeId)
  fromEdges xs = setUniqueAttrs uniques
    . UniqueGraph
    $ G.fromEdges (unUnique <$> xs)
    where
      uniques = concatMap (foldr (:) []) xs
  succs n (UniqueGraph g) =
    getUniqueSet g $ G.succs (n ^. #nodeId) g
  preds n (UniqueGraph g) =
    getUniqueSet g $ G.preds (n ^. #nodeId) g
  nodes (UniqueGraph g) = getUniqueSet g $ G.nodes g
  edges (UniqueGraph g) = fmap (fmap $ getUnique g) $ G.edges g
  getEdgeLabel e (UniqueGraph g) = G.getEdgeLabel (unUnique e) g
  setEdgeLabel lbl e (UniqueGraph g) = UniqueGraph $ G.setEdgeLabel lbl (unUnique e) g
  getNodeAttr _ _ = Just ()
  setNodeAttr _ _ g = g
  removeEdge e (UniqueGraph g) = UniqueGraph $ G.removeEdge (unUnique e) g
  removeNode n (UniqueGraph g) = UniqueGraph $ G.removeNode (n ^. #nodeId) g
  addNodes xs (UniqueGraph g) = setUniqueAttrs xs
    . UniqueGraph
    . G.addNodes (fmap (view #nodeId) xs)
    $ g
  addNodesWithAttrs _ g = g
  addEdge e (UniqueGraph g) = setUniqueAttrs e
    . UniqueGraph
    $ G.addEdge (unUnique e) g
  hasNode n (UniqueGraph g) = G.hasNode (n ^. #nodeId) g
  transpose (UniqueGraph g) = UniqueGraph $ G.transpose g
  bfs xs (UniqueGraph g) = fmap (getUnique g) <$> G.bfs (view #nodeId <$> xs) g
  subgraph p (UniqueGraph g) = UniqueGraph
    . G.subgraph (p . getUnique g)
    $ g

updateNode :: (n -> n) -> Unique n -> UniqueGraph e n -> UniqueGraph e n
updateNode f n (UniqueGraph g) = UniqueGraph
  $ G.updateNodeAttr f (n ^. #nodeId) g

setNode :: n -> Unique n -> UniqueGraph e n -> UniqueGraph e n
setNode n' n g = updateNode (const n') n g
