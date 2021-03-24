module Blaze.Fir where

import Blaze.Prelude
import Blaze.Types.Fir hiding (dest)
import Blaze.Types.Graph (Edge(Edge), Graph, LEdge(LEdge))
import qualified Blaze.Types.Graph as G
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


getNodesWithTwoChildren :: forall e attr n g. (Graph e attr n g) => g -> [(n, (n, n))]
getNodesWithTwoChildren g = mapMaybe f . Set.toList . G.nodes $ g
  where
    f :: n -> Maybe (n, (n, n))
    f p = case Set.toList (G.succs p g) of
      [c1, c2] -> Just (p, (c1, c2))
      _ -> Nothing

chainMapOrigins :: (Ord a) => Map a a -> Set a
chainMapOrigins hm = keySet `Set.difference` elemSet
  where
    elemSet = Set.fromList $ Map.elems hm
    keySet = Set.fromList $ Map.keys hm

getChain :: (Ord a) => Map a a -> a -> [a]
getChain hm origin = case Map.lookup origin hm of
  Nothing -> [origin]
  (Just next) -> origin : getChain hm next

chainMapToLists :: (Ord n) => Map n n -> [[n]]
chainMapToLists hm = fmap (getChain hm) . Set.toList $ chainMapOrigins hm

getIfChainNodes :: forall e attr n g. ( Graph e attr n g, Ord n )
                => g -> [IfChainNode n]
getIfChainNodes g = do
  (n, (c1, c2)) <- twoKidsList
  maybe [] return $ do
    p <- case Set.toList $ G.preds n g of
      [p] -> return p
      _ -> Nothing
    (pkid1, pkid2) <- Map.lookup p twoKidsMap
    let cnode cEsc dest = IfChainNode { _commonEscape = cEsc
                                      , _destination = dest
                                      , _self = n
                                      , _parent = p
                                      }
    case (c1 == pkid1 || c1 == pkid2, c2 == pkid1 || c2 == pkid2) of
      (True, False) -> return $ cnode c1 c2
      (False, True) -> return $ cnode c2 c1
      _ -> Nothing  
  where
    twoKidsList :: [(n, (n, n))]
    twoKidsList = getNodesWithTwoChildren g

    twoKidsMap :: Map n (n, n)
    twoKidsMap = Map.fromList twoKidsList


getIfChains :: forall e attr n g.
               ( Graph e attr n g, Ord n )
            => g -> [IfChain n]
getIfChains g = do
  xs <- chainMapToLists chainMapping
  maybeToList $ convertChainList xs
  where
    chainNodes = getIfChainNodes g    

    convertChainList :: [n] -> Maybe (IfChain n)
    convertChainList xs = do
      lastNode <- lastMay xs
      lastIfChainNode <- Map.lookup lastNode chainNodeMap
      let dest = view destination (lastIfChainNode :: IfChainNode n)
          esc = view commonEscape (lastIfChainNode :: IfChainNode n)
      -- restNodes <- getRestNodes firstNode (drop 1 xs)
      return $ IfChain { _commonEscape = esc
                       , _destination = dest
                       , _nodes = xs
                       }

    chainNodeMap :: Map n (IfChainNode n)
    chainNodeMap = Map.fromList . fmap (\n -> (view self n, n)) $ chainNodes
    
    chainMapping :: Map n n
    chainMapping = Map.fromList . fmap f $ chainNodes
      where
        f n = (view parent n, view self n)

toFirGraph :: forall e attr n g g'.
              ( Graph e attr n g, Ord n, Graph (FirEdgeLabel e) attr (FirNode n) g')
           => [IfChain n] -> g -> g'
toFirGraph ifChains g = case Set.toList $ G.nodes g of
  [bb] -> G.fromNode $ FirBasicBlock bb
  _ -> G.fromEdges . (ifChainEdges <>) $ do
    (G.LEdge e (G.Edge src dst)) <- G.edges g
    guard . not $ Set.member src nodeSet
    case Map.lookup dst startNodeMap of
      Nothing -> return (LEdge (NormalEdge e) (Edge (FirBasicBlock src) (FirBasicBlock dst)))
      (Just ifc) -> return
        $ LEdge (NormalEdge e) (Edge (FirBasicBlock src) (FirIfChain ifc))
  where
    nodeSet :: Set n
    nodeSet = Set.fromList $ concatMap (view nodes) ifChains

    ifChainEdges :: [G.LEdge (FirEdgeLabel e) (FirNode n)]
    ifChainEdges = do
      ifc <- ifChains
      [ LEdge ChainEscapeEdge
        $ Edge (FirIfChain ifc) (maybeSwap $ ifc ^. commonEscape)
        , LEdge ChainDestinationEdge
          $ Edge (FirIfChain ifc) (maybeSwap $ ifc ^. destination)
        ]

    maybeSwap :: n -> FirNode n
    maybeSwap n = maybe (FirBasicBlock n) FirIfChain $ Map.lookup n startNodeMap

    startNodeMap :: Map n (IfChain n)
    startNodeMap = Map.fromList $ mapMaybe f ifChains
      where
        f ifc = (,ifc) <$> headMay (ifc ^. nodes)
