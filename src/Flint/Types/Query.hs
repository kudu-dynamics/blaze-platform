{- HLINT ignore "Use newtype instead of data" -}
module Flint.Types.Query
  ( module Flint.Types.Query
  ) where

import Flint.Prelude

import Flint.Analysis.Path.Matcher (StmtPattern, BoundText)

import Blaze.Import.CallGraph (CallGraphImporter)
import qualified Blaze.Import.CallGraph as Cg
import Blaze.Types.Function (Function)
import Blaze.Types.Graph.Alga (AlgaGraph)
import qualified Blaze.Types.Graph as G

import qualified Data.HashSet as HashSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text


-- | Types used for query/search
data BugMatch = BugMatch
  { pathPattern :: [StmtPattern]
  , bugName :: Text
  , bugDescription :: BoundText
  , mitigationAdvice :: BoundText
  } deriving (Eq, Ord, Show, Generic)

data BinarySearchConfig imp func = BinarySearchConfig
  { excludeFuncsFromStore :: [func]
  , binaryPath :: FilePath
  -- | runs the query to get paths, then the PathMatch to look for bugs in paths
  , queries :: [(Query func, [BugMatch])]
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  
data QueryTargetOpts func = QueryTargetOpts
  { start :: func
  , mustReachSome :: NonEmpty (func, Address) -- addr inside any basic block
  , callExpandDepthLimit :: Word64
  , numSamples :: Word64
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

-- | This just picks one callsite to expand until it reaches a dead-end or its limit.
data QueryExploreDeepOpts func = QueryExploreDeepOpts
  { start :: func
  , callExpandDepthLimit :: Word64
  , numSamples :: Word64
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data QueryExpandAllOpts func = QueryExpandAllOpts
  { start :: func
  , callExpandDepthLimit :: Word64
  , numSamples :: Word64
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

-- leaving this `data` for now bc we'll probably expand opts
data QueryAllPathsOpts func = QueryAllPathsOpts
  { start :: func
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data QueryCallSeqOpts func = QueryCallSeqOpts
  { start :: func
  , callSeqPrep :: CallSeqPrep
  , numSamples :: Word64
  , callExpandDepthLimit :: Word64
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data Query func
  = QueryTarget (QueryTargetOpts func)
  | QueryExpandAll (QueryExpandAllOpts func)
  | QueryExploreDeep (QueryExploreDeepOpts func)
  | QueryAllPaths (QueryAllPathsOpts func)
  | QueryCallSeq (QueryCallSeqOpts func)
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

-- | Used in the query config to identify a function by name or addr
data FuncConfig
  = FuncSym Text
  | FuncAddr Address
  deriving (Eq, Ord, Show, Generic)

-- | Gets a function for the query config.
--   If it can't find it, it should immediately report an error.
class GetFunction x where
  getFunction :: CallGraphImporter imp => imp -> x -> IO Function

instance GetFunction Function where
  getFunction _ = return

instance GetFunction Address where
  getFunction imp addr = Cg.getFunction imp addr >>= \case
    Nothing -> error $ "Could not find function at " <> show addr
    Just func -> return func

instance GetFunction Text where
  getFunction imp name = do
    funcs <- Cg.getFunctions imp
    case filter (\fn -> fn ^. #name == name) funcs of
      [] -> error $ "Could not find a function named " <> show name
      [x] -> return x
      _ -> error $ "Found more than one function named " <> show name

instance GetFunction FuncConfig where
  getFunction imp (FuncSym t) = getFunction imp t
  getFunction imp (FuncAddr x) = getFunction imp x

data FuncNode func = FuncNode
  { function :: func
  , nodeId :: Int
  } deriving (Eq, Ord, Hashable, Show, Generic, Functor, Foldable, Traversable)

instance G.Identifiable (FuncNode func) Int where
  getNodeId = G.NodeId . view #nodeId

-- TODO: This just captures Call patterns.
-- We might need to also handle EnterFunc and LeaveFunc patterns if those
-- ever end up being useful.
type CallSequenceGraph func = AlgaGraph () Int (FuncNode func)

type CallSeq = NonEmpty Function

showCallSeq :: CallSeq -> Text
showCallSeq (x :| []) = x ^. #name
showCallSeq (x :| (y:ys)) = (x ^. #name) <> " -> " <> showCallSeq (y :| ys)

data CallSeqPrep = CallSeqPrep
  { canReach :: HashSet Function -- functions that reach every func in callSeq
                                 -- but not necessarily in order, so there will be
                                 -- false positives
  , callSet :: HashSet Function  -- calls in callSeq as a set
  , firstCall :: Function
  , lastCall :: Function
  , callSeq :: NonEmpty Function
  } deriving (Eq, Ord, Show, Generic)

-- instance Tokenizable CallSeqPrep where
--   tokenize x = 

-- for debugging
showCallSeqPrep :: CallSeqPrep -> Text
showCallSeqPrep x = "CallSeqPrep\n"
  <> " { canReach = #{" <> Text.intercalate ", " (fmap (view #name) . HashSet.toList $ x ^. #canReach) <> "}\n"
  <> " , callSet = #{" <> Text.intercalate ", " (fmap (view #name) . HashSet.toList $ x ^. #callSet) <> "}\n"
  <> " , firstCall = " <> x ^. #firstCall . #name <> "\n"
  <> " , lastCall = " <> x ^. #lastCall . #name <> "\n"
  <> " , callSeq = [" <> Text.intercalate ", " (fmap (view #name) . NE.toList $ x ^. #callSeq) <> "]}\n"
  <> " }"
