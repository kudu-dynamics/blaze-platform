module Flint.Types.CachedCalc ( module Flint.Types.CachedCalc ) where

import Flint.Prelude hiding (get)
import qualified Data.HashMap.Strict as HashMap
import Control.Concurrent.STM.TVar (TVar, writeTVar, readTVar, readTVarIO, newTVar)
import Control.Concurrent.STM.TMVar (TMVar, readTMVar, newEmptyTMVar, putTMVar)


-- | This module was taken from Blaze-UI and slightly modified

asyncAndLink_ :: IO a -> IO ()
asyncAndLink_ = link <=< async

-- TODO: add option to persist to db
-- Lazy calc. It will store the calc as a `Left (IO v)` until needed, then will
-- switch over to `Right (TMVar v)` to finish calculating. The first thread to
-- try to access a var sets it to `Right emptyTMVar` then must run the `IO v`
-- and finally place `v` in the TMVar.
newtype CachedCalc k v = CachedCalc
  { cache :: TVar (HashMap k (Either (IO v) (TMVar v)))
  } deriving (Generic)

create :: STM (CachedCalc k v)
create = CachedCalc <$> newTVar HashMap.empty

-- | Gets all keys for which a calc has been set
getKeys :: CachedCalc k v -> IO [k]
getKeys (CachedCalc cc) = HashMap.keys <$> readTVarIO cc

-- | Creates a key in the cache and a calc `IO v`, which will be
-- computed lazily upon the first call to `getCalc` for that key
-- This is intended to be run once per key.
-- If `setCalc` is called on a key that has been or is being calc'ed,
-- it will overwrite it.
setCalc :: Hashable k => k -> CachedCalc k v -> IO v -> IO ()
setCalc k (CachedCalc cc) calc = atomically $ do
  m <- readTVar cc
  writeTVar cc $ HashMap.insert k (Left calc) m

-- | Adds a calculation on top of an existing calculation.
-- First arg passed into `f` is Nothing if key doesn't yet exist.
modifyCalc :: Hashable k => (Maybe v -> IO v) -> k -> CachedCalc k v -> IO ()
modifyCalc f k (CachedCalc cc) = do
  atomically $ do
    m <- readTVar cc
    case HashMap.lookup k m of
      Nothing -> writeTVar cc $ HashMap.insert k (Left $ f Nothing) m
      Just (Left action) -> do
        let action' = action >>= f . Just
        writeTVar cc $ HashMap.insert k (Left action') m
      Just (Right tmv) -> do
        let action = atomically (readTMVar tmv) >>= f . Just
        writeTVar cc $ HashMap.insert k (Left action) m

-- | Retrieves the cached calc. Returns Nothing if the key cannot be found.
-- Otherwise, it will either return `v` or calculate `IO v` then return `v`
get :: Hashable k => k -> CachedCalc k v -> IO (Maybe v)
get k (CachedCalc cc) = do
  mer <- atomically $ do
    m <- readTVar cc
    case HashMap.lookup k m of
      Nothing -> return Nothing
      Just (Left calc) -> do
        tmv <- newEmptyTMVar
        writeTVar cc $ HashMap.insert k (Right tmv) m
        return . Just $ Left (calc, tmv)
      Just (Right tmv) -> return . Just $ Right tmv
  case mer of
    -- Key not found:
    Nothing -> return Nothing
    -- Already calc'ed or being calc'ed, so wait for result:
    Just (Right tmv) -> fmap Just . atomically $ readTMVar tmv
    -- This is the first time this key was opened, so calc and put in TMVar when done:
    Just (Left (calc, tmv)) -> do
      v <- calc
      atomically $ putTMVar tmv v
      return $ Just v

-- | Completely calcs current state of CachedCalc into reified map
getSnapshot :: Hashable k => CachedCalc k v -> IO (HashMap k v)
getSnapshot fullCc@(CachedCalc cc) = do
  keys <- HashMap.keys <$> readTVarIO cc
  fmap HashMap.fromList . mapMaybeConcurrently getVal $ keys
  -- fmap HashMap.fromList . mapMaybeM getVal $ keys
  where
    getVal k = fmap (k,) <$> get k fullCc
