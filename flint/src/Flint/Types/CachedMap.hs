module Flint.Types.CachedMap where

import Flint.Prelude hiding (get, modify)
import qualified Data.HashMap.Strict as HashMap
import Control.Concurrent.STM.TVar (writeTVar, readTVar, readTVarIO, newTVar)


-- | Map that can be asynchronously updated and has a default empty value

data CachedMap k v = CachedMap
  { defaultValue :: v
  , cache :: TVar (HashMap k (TVar v))
  } deriving (Generic)

create :: v -> STM (CachedMap k v)
create defaultVal = CachedMap defaultVal <$> newTVar HashMap.empty

-- | Gets all keys for which a calc has been set
getKeys :: CachedMap k v -> IO [k]
getKeys (CachedMap _ cc) = HashMap.keys <$> readTVarIO cc

-- | Sets the value of a key. If the TVar exists, it sets that TVar instead of
-- creating a new one.
set :: Hashable k => k -> v -> CachedMap k v -> IO ()
set k v (CachedMap _ cc) = atomically $ do
  m <- readTVar cc
  case HashMap.lookup k m of
    Nothing -> do
      tv <- newTVar v
      writeTVar cc $ HashMap.insert k tv m
    Just tv -> writeTVar tv v

get_ :: Hashable k => k -> CachedMap k v -> STM (TVar v)
get_ k (CachedMap defV cc) = do
  m <- readTVar cc
  case HashMap.lookup k m of
    Just tv -> return tv
    Nothing -> do
      tv <- newTVar defV
      writeTVar cc $ HashMap.insert k tv m
      return tv

get :: Hashable k => k -> CachedMap k v -> IO v
get k cc = atomically $ get_ k cc >>= readTVar

modify :: Hashable k => (v -> v) -> k -> CachedMap k v -> IO v
modify f k cc = atomically $ do
  tv <- get_ k cc
  v <- readTVar tv
  let v' = f v
  writeTVar tv v'
  return v

modify_ :: Hashable k => (v -> v) -> k -> CachedMap k v -> IO ()
modify_ f k = void . modify f k

-- alter :: Hashable k => (Maybe v -> Maybe v) -> k -> CachedMap k v -> IO ()
-- alter f k cc = atomically $ do
--   m <- readTVar cc
--   case HashMap.lookup k m of
--     Nothing -> do
--       tv <- newTVar v
--       writeTVar cc $ HashMap.insert k tv m
--     Just tv -> writeTVar tv v


-- -- | Retrieves the cached calc. Returns Nothing if the key cannot be found.
-- -- Otherwise, it will either return `v` or calculate `IO v` then return `v`
-- get :: Hashable k => k -> CachedMap k v -> IO (Maybe v)
-- get k (CachedMap cc) = do
--   mer <- atomically $ do
--     m <- readTVar cc
--     case HashMap.lookup k m of
--       Nothing -> return Nothing
--       Just (Left calc) -> do
--         tmv <- newEmptyTMVar
--         writeTVar cc $ HashMap.insert k (Right tmv) m
--         return . Just $ Left (calc, tmv)
--       Just (Right tmv) -> return . Just $ Right tmv
--   case mer of
--     -- Key not found:
--     Nothing -> return Nothing
--     -- Already calc'ed or being calc'ed, so wait for result:
--     Just (Right tmv) -> fmap Just . atomically $ readTMVar tmv
--     -- This is the first time this key was opened, so calc and put in TMVar when done:
--     Just (Left (calc, tmv)) -> do
--       v <- calc
--       atomically $ putTMVar tmv v
--       return $ Just v

-- | Completely calcs current state of CachedMap into reified map
getSnapshot :: forall k v. Hashable k => CachedMap k v -> IO (HashMap k v)
getSnapshot fullCc@(CachedMap _ cc) = do
  keys <- HashMap.keys <$> readTVarIO cc
  fmap HashMap.fromList . mapConcurrently getVal $ keys
  where
    getVal :: k -> IO (k, v)
    getVal k = (k,) <$> get k fullCc

-- | Totally replaces values in CachedMap
putSnapshot
  :: forall k v. Hashable k
  => HashMap k v
  -> CachedMap k v
  -> IO ()
putSnapshot m (CachedMap _ cc) = atomically $ do
  m' <- fmap HashMap.fromList . forM (HashMap.toList m)
       $ \(k, v) -> (k,) <$> newTVar v
  writeTVar cc m'
