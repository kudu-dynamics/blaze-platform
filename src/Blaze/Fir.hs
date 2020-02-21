module Blaze.Fir where

import qualified Prelude as P
import           Blaze.Prelude

import Data.Map ((!))
import           Binja.BasicBlock                  ( BasicBlock
                                                   , BasicBlockFunction
                                                   , BlockEdge
                                                   )
import qualified Binja.BasicBlock     as BB
import           Binja.C.Enums                     ( BNBranchType( FalseBranch
                                                                 , TrueBranch
                                                                 )
                                                   )
import           Binja.Core                        ( BNBinaryView
                                                   , InstructionIndex(InstructionIndex)
                                                   
                                                   )
import           Binja.Function                    ( Function
                                                   , MLILSSAFunction
                                                   )
import qualified Binja.Function       as HFunction
import qualified Binja.MLIL           as MLIL
import           Blaze.Function                    ( createCallSite )
import qualified Blaze.Function       as Function
import           Blaze.Graph.Alga                  ( AlgaGraph )
import           Blaze.Types.Function              ( CallInstruction
                                                   , CallSite
                                                   , toCallInstruction
                                                   )
import           Blaze.Types.Graph                 ( Graph )
import qualified Blaze.Types.Graph    as G
import qualified Data.Set as Set
import qualified Streamly.Prelude as S
import Blaze.Types.Path (ConditionNode(ConditionNode))
import qualified Blaze.Graph as Graph
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


type F = MLILSSAFunction

getNodesWithTwoChildren :: forall e n g. (Graph e n g, Ord n) => g -> [(n, (n, n))]
getNodesWithTwoChildren g = mapMaybe f . Set.toList . G.nodes $ g
  where
    f :: n -> Maybe (n, (n, n))
    f p = case Set.toList (G.succs p g) of
      [c1, c2] -> Just (p, (c1, c2))
      _ -> Nothing



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


type ChainMapping n = Map (ChainNode n) (ChainNode n)

chainMapOrigins :: (Ord a, Eq a) => Map a a -> Set a
chainMapOrigins hm = keySet `Set.difference` elemSet
  where
    elemSet = Set.fromList $ Map.elems hm
    keySet = Set.fromList $ Map.keys hm

demoh :: Map Int Int
demoh = Map.fromList
  [ (1, 2)
  , (2, 3)
  , (3, 4)
  , (4, 5)
  , (5, 6)
  , (7, 8)
  , (8, 9)
  ]

getChain :: (Ord a, Eq a) => Map a a -> a -> [a]
getChain hm origin = case Map.lookup origin hm of
  Nothing -> [origin]
  (Just next) -> origin : getChain hm next

chainMapToLists :: (Ord n, Eq n) => Map n n -> [[n]]
chainMapToLists hm = fmap (getChain hm) . Set.toList $ chainMapOrigins hm

getIfChainNodes :: forall e n g. ( Graph e n g, Ord n )
                => g -> [IfChainNode n]
getIfChainNodes g = do
  (n, (c1, c2)) <- twoKidsList
  maybe [] return $ do
    p <- case Set.toList $ G.preds n g of
      [p] -> return p
      _ -> Nothing
    (pkid1, pkid2) <- Map.lookup p twoKidsMap
    let cnode cEsc dest = IfChainNode { commonEscape = cEsc
                                      , destination = dest
                                      , self = n
                                      , parent = p
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


data IfChain n = IfChain
  { commonEscape :: n
  , destination :: n
  , nodes :: [n]
  } deriving (Eq, Ord, Show, Generic)

instance Hashable n => Hashable (IfChain n)


getIfChains :: forall e n g.
               ( Graph e n g, Ord n )
            => g -> [IfChain n]
getIfChains g = do
  xs <- chainMapToLists chainMapping
  maybe [] (:[]) $ convertChainList xs
  where
    chainNodes = getIfChainNodes g    

    convertChainList :: [n] -> Maybe (IfChain n)
    convertChainList xs = do
      lastNode <- lastMay xs
      lastIfChainNode <- Map.lookup lastNode chainNodeMap
      let dest = destination (lastIfChainNode :: IfChainNode n)
          esc = commonEscape (lastIfChainNode :: IfChainNode n)
      -- restNodes <- getRestNodes firstNode (drop 1 xs)
      return $ IfChain { commonEscape = esc
                       , destination = dest
                       , nodes = xs
                       }

    chainNodeMap :: Map n (IfChainNode n)
    chainNodeMap = Map.fromList . fmap (\n -> (self n, n)) $ chainNodes
    
    chainMapping :: Map n n
    chainMapping = Map.fromList . fmap f $ chainNodes
      where
        f n = (parent n, self n)
      
    
