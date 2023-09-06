module Flint.Types.Query
  ( module Flint.Types.Query
  ) where

import Flint.Prelude

import Blaze.Types.Function (Function)
import Blaze.Import.CallGraph (CallGraphImporter)
import qualified Blaze.Import.CallGraph as Cg


-- | Types used for query/search

data BinarySearchConfig imp func = BinarySearchConfig
  { excludeFuncsFromStore :: [func]
  , binaryPath :: FilePath
  , queries :: [Query func]
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  
data QueryTargetOpts func = QueryTargetOpts
  { start :: func
  , mustReachSome :: NonEmpty (func, Address) -- addr inside any basic block
  , callExpandDepthLimit :: Word64
  , numSamples :: Word64
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data QueryExpandAllOpts func = QueryExpandAllOpts
  { start :: func
  , callExpandDepthLimit :: Word64
  , numSamples :: Word64
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data Query func
  = QueryTarget (QueryTargetOpts func)
  | QueryExpandAll (QueryExpandAllOpts func)
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
    case filter (\fn -> fn ^. #name == name) $ funcs of
      [] -> error $ "Could not find a function named " <> show name
      [x] -> return x
      _ -> error $ "Found more than one function named " <> show name

instance GetFunction FuncConfig where
  getFunction imp (FuncSym t) = getFunction imp t
  getFunction imp (FuncAddr x) = getFunction imp x
