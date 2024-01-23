module Blaze.Fir where

import Blaze.Prelude
import Blaze.Types.Fir hiding (dest)
import Blaze.Types.Graph (Edge(Edge), Graph, GraphConstruct, LEdge(LEdge))
import qualified Blaze.Types.Graph as G
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet


getNodesWithTwoChildren :: forall l n g. (Hashable n, Graph l n g) => g n -> [(n, (n, n))]
getNodesWithTwoChildren g = mapMaybe f . HashSet.toList . G.nodes $ g
  where
    f :: n -> Maybe (n, (n, n))
    f p = case HashSet.toList (G.succs p g) of
      [c1, c2] -> Just (p, (c1, c2))
      _ -> Nothing

chainMapOrigins :: Hashable a => HashMap a a -> HashSet a
chainMapOrigins hm = keySet `HashSet.difference` elemSet
  where
    elemSet = HashSet.fromList $ HashMap.elems hm
    keySet = HashSet.fromList $ HashMap.keys hm

getChain :: Hashable a => HashMap a a -> a -> [a]
getChain hm origin = case HashMap.lookup origin hm of
  Nothing -> [origin]
  (Just next) -> origin : getChain hm next

chainMapToLists :: Hashable n => HashMap n n -> [[n]]
chainMapToLists hm = fmap (getChain hm) . HashSet.toList $ chainMapOrigins hm

getIfChainNodes :: forall l n g. ( Graph l n g, Hashable n)
                => g n -> [IfChainNode n]
getIfChainNodes g = do
  (n, (c1, c2)) <- twoKidsList
  maybe [] return $ do
    p <- case HashSet.toList $ G.preds n g of
      [p] -> return p
      _ -> Nothing
    (pkid1, pkid2) <- HashMap.lookup p twoKidsMap
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

    twoKidsMap :: HashMap n (n, n)
    twoKidsMap = HashMap.fromList twoKidsList


getIfChains :: forall l n g.
               ( Graph l n g
               , Hashable n
               )
            => g n -> [IfChain n]
getIfChains g = do
  xs <- chainMapToLists chainMapping
  maybeToList $ convertChainList xs
  where
    chainNodes = getIfChainNodes g    

    convertChainList :: [n] -> Maybe (IfChain n)
    convertChainList xs = do
      lastNode <- lastMay xs
      lastIfChainNode <- HashMap.lookup lastNode chainNodeMap
      let dest = view destination (lastIfChainNode :: IfChainNode n)
          esc = view commonEscape (lastIfChainNode :: IfChainNode n)
      -- restNodes <- getRestNodes firstNode (drop 1 xs)
      return $ IfChain { _commonEscape = esc
                       , _destination = dest
                       , _nodes = xs
                       }

    chainNodeMap :: HashMap n (IfChainNode n)
    chainNodeMap = HashMap.fromList . fmap (\n -> (view self n, n)) $ chainNodes
    
    chainMapping :: HashMap n n
    chainMapping = HashMap.fromList . fmap f $ chainNodes
      where
        f n = (view parent n, view self n)

toFirGraph :: forall l n g g'.
              ( Graph l n g
              , Hashable n
              , GraphConstruct (FirEdgeLabel l) (FirNode n) g'
              )
           => [IfChain n] -> g n -> g' (FirNode n)
toFirGraph ifChains g = case HashSet.toList $ G.nodes g of
  [bb] -> G.fromNode $ FirBasicBlock bb
  _ -> G.fromEdges . (ifChainEdges <>) $ do
    (G.LEdge e (G.Edge src dst)) <- G.edges g
    guard . not $ HashSet.member src nodeSet
    case HashMap.lookup dst startNodeMap of
      Nothing -> return (LEdge (Standard e) (Edge (FirBasicBlock src) (FirBasicBlock dst)))
      (Just ifc) -> return
        $ LEdge (Standard e) (Edge (FirBasicBlock src) (FirIfChain ifc))
  where
    nodeSet :: HashSet n
    nodeSet = HashSet.fromList $ concatMap (view nodes) ifChains

    ifChainEdges :: [G.LEdge (FirEdgeLabel e) (FirNode n)]
    ifChainEdges = do
      ifc <- ifChains
      [ LEdge ChainEscapeEdge
        $ Edge (FirIfChain ifc) (maybeSwap $ ifc ^. commonEscape)
        , LEdge ChainDestinationEdge
          $ Edge (FirIfChain ifc) (maybeSwap $ ifc ^. destination)
        ]

    maybeSwap :: n -> FirNode n
    maybeSwap n = maybe (FirBasicBlock n) FirIfChain $ HashMap.lookup n startNodeMap

    startNodeMap :: HashMap n (IfChain n)
    startNodeMap = HashMap.fromList $ mapMaybe f ifChains
      where
        f ifc = (,ifc) <$> headMay (ifc ^. nodes)
