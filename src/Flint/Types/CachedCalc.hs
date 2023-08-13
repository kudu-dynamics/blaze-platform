module Flint.Types.CachedCalc where

import Flint.Prelude
import qualified Data.HashMap.Strict as HashMap
import Control.Concurrent.STM.TVar (TVar, readTVarIO, writeTVar, readTVar, newTVar)
import Control.Concurrent.STM.TMVar (TMVar, readTMVar, tryPutTMVar, newEmptyTMVar, putTMVar)

-- | This module was taken from Blaze-UI

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

-- -- | Creates empty TMVar for k, calculates action in thread,
-- -- then inserts new v into TMVar.
-- -- The calcuation is stored as a `Left (IO v)` until something actually needs it,
-- -- then it is calculated.
-- setCalc :: Hashable k => k -> CachedCalc k v -> IO v -> IO (TVar v)
-- setCalc k (CachedCalc cc) action = do
--   tmvar <- atomically $ do
--     m <- readTVar cc
--     case HashMap.lookup k m of
--       Just v -> return v
--       Nothing -> do
--         emptyV <- newEmptyTMVar
--         writeTVar cc $ HashMap.insert k emptyV m
--         return emptyV
--   -- Currently this is called through a warp webserver thread or through an
--   -- event handler thread in Blaze.UI.Server, which both can handle it.
--   asyncAndLink_ $ do
--     v <- action
--     void . atomically $ tryPutTMVar tmvar v
--   return tmvar

-- | Creates a key in the cache and a calc `IO v`, which will be
-- computed lazily upon the first call to `getCalc` for that key
-- This is intended to be run once per key.
-- If `setCalc` is called on a key that has been or is being calc'ed,
-- it will overwrite it.
setCalc :: Hashable k => k -> CachedCalc k v -> IO v -> IO ()
setCalc k (CachedCalc cc) calc = atomically $ do
  m <- readTVar cc
  writeTVar cc $ HashMap.insert k (Left calc) m

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

-- -- | Retrieves the cached calc or computes it and caches it.
-- -- Blocks thread until return.
-- calc :: Hashable k => k -> CachedCalc k v -> IO v -> IO v
-- calc k cc action = getCalc k cc >>= \case
--   Just v -> return v
--   Nothing -> setCalc k cc action >>= atomically . readTMVar
