module Flint.Prelude
  ( module Exports
  , catHashMapMaybes
  , constrName
  , hoistMaybeM
  , mapMaybeConcurrently
  , mapMaybeHashSet
  , putText
  , removeNth
  , tryError
  ) where

-- We hide Protolude's @putText@ (via Blaze.Prelude) and shadow it with one
-- that writes to stderr. Rationale: flint-mcp uses stdout as the JSON-RPC
-- frame channel, so ANY non-JSON output on stdout — including analysis
-- progress messages from deep in the library — corrupts the protocol and
-- breaks the MCP client. Routing @putText@ through stderr keeps the shell
-- experience identical on a terminal (stdout+stderr interleave visually)
-- while making flint-mcp safe.
import Blaze.Prelude as Exports hiding (Symbol, putText)

import Control.Concurrent.Async as Exports (replicateConcurrently, forConcurrently, forConcurrently_, mapConcurrently_)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text.IO as TIO
import Data.Vector as Exports (Vector)

putText :: MonadIO m => Text -> m ()
putText = liftIO . TIO.hPutStrLn stderr


hoistMaybeM :: Monad m => m (Maybe a) -> MaybeT m a
hoistMaybeM m = lift m >>= maybe mzero return

removeNth :: Int -> [a] -> [a]
removeNth 0 [] = []
removeNth 0 (_:xs) = xs
removeNth _ [] = []
removeNth n (x:xs) = x : removeNth (n - 1) xs

-- | 'MonadError' analogue to the 'Control.Exception.try' function.
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)


----- Generic constructor names
constrName :: (HasConstructor (Rep a), Generic a) => a -> Text
constrName = cs . genericConstrName . from

class HasConstructor (f :: Type -> Type) where
  genericConstrName :: f x -> String

instance HasConstructor f => HasConstructor (D1 c f) where
  genericConstrName (M1 x) = genericConstrName x

instance (HasConstructor x, HasConstructor y) => HasConstructor (x :+: y) where
  genericConstrName (L1 l) = genericConstrName l
  genericConstrName (R1 r) = genericConstrName r

instance Constructor c => HasConstructor (C1 c f) where
  genericConstrName = conName

mapMaybeConcurrently :: (a -> IO (Maybe b)) -> [a] -> IO [b]
mapMaybeConcurrently f = fmap catMaybes . mapConcurrently f

catHashMapMaybes :: Hashable k => HashMap k (Maybe v) -> HashMap k v
catHashMapMaybes = HashMap.fromList . mapMaybe f . HashMap.toList
  where
    f (_, Nothing) = Nothing
    f (k, Just v) = Just (k, v)

mapMaybeHashSet :: Hashable b => (a -> Maybe b) -> HashSet a -> HashSet b
mapMaybeHashSet f = HashSet.fromList . mapMaybe f . HashSet.toList
