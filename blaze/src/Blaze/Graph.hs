module Blaze.Graph
  ( module Exports,
    module Blaze.Graph,
  )
where

import Blaze.Prelude
import qualified Blaze.Types.Graph as G
import Blaze.Types.Graph as Exports hiding (edge, label, src, dst)
import qualified Blaze.Types.Graph.EdgeGraph as Eg
import Blaze.Types.Graph.EdgeGraph (EdgeGraphNode (NodeNode, EdgeNode))
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HSet


------- Dominators
getDominatorMapping
  :: forall n g l. (Hashable n, Graph l n g)
  => n
  -> g n
  -> HashMap n (HashSet n)
getDominatorMapping rootNode g = foldl' (flip buildDominatedMapping) HashMap.empty allNodes
  where
    allNodes = reachable rootNode g
    allNodesSet = HashSet.fromList allNodes
    getDominatedBy :: n -> [n]
    getDominatedBy node = HashSet.toList
      . HashSet.difference allNodesSet
      . HashSet.fromList
      . bfsReachable rootNode
      . removeNode node
      $ g
    buildDominatedMapping :: n -> HashMap n (HashSet n) -> HashMap n (HashSet n)
    buildDominatedMapping n m = foldr alterIfNotEqual m $ getDominatedBy n
      where
        alterIfNotEqual :: n -> HashMap n (HashSet n) -> HashMap n (HashSet n)
        alterIfNotEqual n' m'
          | n' == n = m'
          | otherwise = HashMap.alter addOrCreate n' m'
        addOrCreate :: Maybe (HashSet n) -> Maybe (HashSet n)
        addOrCreate Nothing = Just $ HashSet.singleton n
        addOrCreate (Just s) = Just $ HashSet.insert n s

-- | Nodes reachable from n in bfs order, excludes self unless can be reached later
bfsReachable :: (Graph l n g) => n -> g n -> [n]
bfsReachable n g = concat $ bfs [n] g

getDominators :: (Hashable n, Graph l n g) => n -> g n -> Dominators n
getDominators rootNode = Dominators . getDominatorMapping rootNode

getPostDominators_ :: (Hashable n, Graph l n g) => n -> g n -> PostDominators n
getPostDominators_ termNode = PostDominators . getDominatorMapping termNode . G.transpose

-- | Gets all post dominators. If there are multiple terminal nodes,
--   each will point to `dummyTermNode`.
getPostDominators
  :: (Hashable n, Graph l n g)
  => n
  -> l
  -> g n
  -> PostDominators n
getPostDominators dummyTermNode dummyTermEdgeLabel g =
  case HashSet.toList $ G.getTermNodes g of
    [] -> domEmpty
    [x] -> getPostDominators_ x g
    xs -> domRemoveNode dummyTermNode
      . getPostDominators_ dummyTermNode
      $ foldl' (flip f) g xs
      where
        f x = G.addEdge (G.LEdge dummyTermEdgeLabel $ G.Edge x dummyTermNode)

-- | All nodes that can reach node and that can be reached by node.
-- Formally, this is the union of the in-component and the out-component of
-- node n in graph g.
connectedNodes :: (Graph l n g, Hashable n) => n -> g n -> HashSet n
connectedNodes n g = HSet.fromList reachedNodes <> HSet.fromList reachingNodes
  where
    reachedNodes = G.reachable n g
    reachingNodes = G.reachable n $ G.transpose g

connectedNodesAndEdges :: forall l n g g'.
  ( Graph l n g
  , Graph () (EdgeGraphNode l n) g'
  , GraphConstruct () (EdgeGraphNode l n) g'
  , Hashable n
  , Hashable l
  ) => Proxy g' -> n -> g n -> (HashSet n, HashSet (LEdge l n))
connectedNodesAndEdges _ n g = foldr f (HSet.empty, HSet.empty) cnodes
  where
    g' = Eg.toEdgeGraph g :: g' (EdgeGraphNode l n)
    cnodes = connectedNodes (NodeNode n) g'
    f :: EdgeGraphNode l n -> (HashSet n, HashSet (LEdge l n)) -> (HashSet n, HashSet (LEdge l n))
    f (NodeNode x) (nodes', edges') = (HSet.insert x nodes', edges')
    f (EdgeNode e) (nodes', edges') = (nodes', HSet.insert e edges')

-- | Supposing each node in a graph contains x's which can be obtained with
-- `getInnerNodes`, this function collects the x's in each node, but also
-- includes x's from nodes it can reach.
-- The second arg should be ancestors, or a reverse DependencyMap
calcOuterNodeDescendantsM
  :: forall m outerNode innerNode. (Hashable outerNode, Hashable innerNode, Monad m)
  => (outerNode -> m (HashSet innerNode))
  -> HashMap outerNode (HashSet outerNode) -- `HashSet n` are ancestors of the key `n`
  -> m (OuterNodeDescendants outerNode innerNode)
calcOuterNodeDescendantsM getInnerNodes ancestorsMap =
  foldM f HashMap.empty . HashMap.keys $ ancestorsMap
  where
    f :: OuterNodeDescendants outerNode innerNode
      -> outerNode
      -> m (OuterNodeDescendants outerNode innerNode)
    f m k = do
      s <- getInnerNodes k
      let ancestors = maybe [] HashSet.toList $ HashMap.lookup k ancestorsMap
      return $ foldl' (flip . HashMap.alter $ addNodes' s) m $ k : ancestors

    addNodes' :: HashSet innerNode -> Maybe (HashSet innerNode) -> Maybe (HashSet innerNode)
    addNodes' s Nothing = Just s
    addNodes' s (Just s') = Just $ HashSet.union s s'

calcOuterNodeDescendants
  :: forall outerNode innerNode. (Hashable outerNode, Hashable innerNode)
  => (outerNode -> HashSet innerNode) -- TODO: make this a hashmap?
  -> HashMap outerNode (HashSet outerNode) -- `HashSet outerNode` are ancestors of the key `outerNode`
  -> OuterNodeDescendants outerNode innerNode
calcOuterNodeDescendants getInnerNodes = runIdentity
  . calcOuterNodeDescendantsM (return . getInnerNodes)

-- | Suppose we have a graph of graphs (like a CallGraph of functions, where each function
-- contains a Cfg, and within that Cfg there are calls to other functions. This function
-- calculates node descendants that span across call sites to other functions.
--
-- Warning: This was created with the Function/Cfg case in mind, where nodes are unique
-- per function (each node as a Ctx unique to its function). This function requires that each
-- innerNode is unique.
calcInterDescendantsMapForCfg
  :: forall outerNode innerNode.
     ( Hashable outerNode
     , Hashable innerNode
     )
  => (innerNode -> Maybe outerNode) -- does node flow to a different outer node? (ie Call node)
  -> StrictDescendantsMap innerNode -- regular descendants map of single Cfg
  -> OuterNodeDescendants outerNode innerNode
  -> InterDescendantsMap innerNode
calcInterDescendantsMapForCfg getTransToOuter (StrictDescendantsMap dmap) outerNodeDescendants =
  foldl' addDescendants mempty $ HashMap.keys dmap
  where
    addDescendants
      :: InterDescendantsMap innerNode
      -> innerNode
      -> InterDescendantsMap innerNode
    addDescendants (InterDescendantsMap m) x = InterDescendantsMap $ HashMap.insert x descs m
      where
        innerDescs = fromJust $ HashMap.lookup x dmap
        outerDescs = mapMaybe getTransToOuter . HashSet.toList $ innerDescs
        getOuterNode'sInnerNodes
          = fromMaybe HashSet.empty
          . flip HashMap.lookup outerNodeDescendants 
        nodesFromOuterDescs
          = foldl' (\s outer -> getOuterNode'sInnerNodes outer <> s) HashSet.empty outerDescs
        descs = nodesFromOuterDescs <> innerDescs
  
makeRoutes_
  :: forall outerContext n.
     ( Hashable outerContext
     , Hashable n
     , Show outerContext
     , Show n
     )
  => RouteMakerCtx outerContext n
  -> Word64                       -- current call depth
  -> outerContext                 -- what "function" are we currently in? 
  -> n                            -- what node in the "cfg" are we currently at?
  -> Bool                         -- just entered context?    
  -> [n]                          -- sequence of required nodes
  -> [(Route outerContext n, [n])]
makeRoutes_ ctx currentCallDepth currentOuterContext currentNode justEnteredContext reqSeq = case reqSeq of
  [] -> return ([Finished], [])
  (req:reqs) -> case HashMap.lookup currentOuterContext $ ctx ^. #getDescendantsMap of
    Nothing -> error $ "Could not find descendant map for outerContext: " <> show currentOuterContext
    Just (StrictDescendantsMap dmap) -> case HashMap.lookup currentNode dmap of
      Nothing -> error "Could not find currentNode in descendantsMap"
      Just descs ->
        let descsList = HashSet.toList descs
                
            transNodesAndContexts = mapMaybe (\n -> (n,) <$> (ctx ^. #getTransNodeContext $ n)) descsList

            transNodesAndContextsThatReachReq :: [(n, outerContext)]
            transNodesAndContextsThatReachReq
              = flip filter transNodesAndContexts
                $ \(_transNode, transCtx) ->
                    case HashMap.lookup transCtx $ ctx ^. #outerContextNodeDescendants of
                      Nothing -> False
                      Just reachableNodes -> HashSet.member req reachableNodes
            plansForInnerReq = bool notFound found $
              justEnteredContext && currentNode == req
              ||
              HashSet.member req descs
              where
                notFound = [] -- [([ExitContext currentOuterContext], req:reqs)] -- or should be just return []?
                found = do
                  (nextRoute, reqs') <- makeRoutes_ ctx currentCallDepth currentOuterContext req False reqs
                  return (InnerNode req : nextRoute, reqs')
            plansForTrans = case currentCallDepth + 1 < ctx ^. #maxCallDepth of
              False -> []
              True -> do
                (transNode, transOuterContext) <- transNodesAndContextsThatReachReq
                let startNode = fromMaybe
                      (error $ "Couldn't find start node for outer context: " <> show transOuterContext)
                      $ HashMap.lookup transOuterContext
                      $ ctx ^. #getStartNode
                makeRoutes_ ctx (currentCallDepth + 1) transOuterContext startNode True (req:reqs) >>= \case
                  (transRoute, []) -> return (EnterContext transNode transOuterContext : transRoute, [])
                    -- Still more in sequence to find in calling context
                  (transRoute, reqs') -> do
                    (nextRoute, reqs'') <- makeRoutes_ ctx currentCallDepth currentOuterContext transNode False reqs'
                    return (EnterContext transNode transOuterContext : transRoute <> nextRoute, reqs'')                      
            allRoutes = plansForInnerReq <> plansForTrans
        in
          case allRoutes of
            [] -> if justEnteredContext
              then []
              else return ([ExitContext currentOuterContext], req:reqs)
            _ -> if justEnteredContext
              then allRoutes
              else ([ExitContext currentOuterContext], req:reqs) : allRoutes

-- | Gets all plans that can follow a given sequence, up to a certain call depth.
-- This is just filters the result of `makeRoutes_` to exclude incomplete matches.
makeRoutes
  :: forall outerContext n.
     ( Hashable outerContext
     , Hashable n
     , Show outerContext
     , Show n
     )
  => RouteMakerCtx outerContext n
  -> outerContext                 -- what "function" are we currently in? 
  -> n                            -- what node in the "cfg" are we currently at?
  -> [n]                          -- sequence of required nodes
  -> [Route outerContext n]
makeRoutes ctx currentOuterContext currentNode reqSeq
  = mapMaybe getCompleteRoute
    $ makeRoutes_
      ctx
      0
      currentOuterContext
      currentNode
      True
      reqSeq
  where
    getCompleteRoute (p, []) = Just p
    getCompleteRoute _ = Nothing
