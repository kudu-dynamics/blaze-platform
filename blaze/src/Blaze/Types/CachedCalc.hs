module Blaze.Types.CachedCalc where

import Prelude (show)
import Blaze.Prelude hiding (get, show)
import qualified Data.HashMap.Strict as HashMap
import Control.Concurrent.STM.TVar (writeTVar, readTVar, readTVarIO, newTVar)
import Control.Concurrent.STM.TMVar (readTMVar, newEmptyTMVar, putTMVar)
import qualified Control.Exception as Ex


data CachedCalc k v = CachedCalc
  { cache :: TVar (HashMap k (Either (IO v) (TMVar (Either Ex.SomeException v))))
  , defaultCalc :: TVar (Maybe (k -> IO v))
  } deriving (Generic)

instance Show (CachedCalc k v) where
  show _ = "<CachedCalc>"

create :: STM (CachedCalc k v)
create = CachedCalc <$> newTVar HashMap.empty <*> newTVar Nothing

-- | Set a default computation for unknown keys. When `get` is called
-- with a key that has no registered calc, it uses this function instead
-- of returning Nothing.
setDefault :: (k -> IO v) -> CachedCalc k v -> IO ()
setDefault f (CachedCalc _ dc) = atomically $ writeTVar dc (Just f)

-- | Gets all keys for which a calc has been set
getKeys :: CachedCalc k v -> IO [k]
getKeys (CachedCalc cc _) = HashMap.keys <$> readTVarIO cc

-- | Creates a key in the cache and a calc `IO v`, which will be
-- computed lazily upon the first call to `get` for that key.
-- This is intended to be run once per key.
-- If `setCalc` is called on a key that has been or is being calc'ed,
-- it will overwrite it.
setCalc :: Hashable k => k -> CachedCalc k v -> IO v -> IO ()
setCalc k (CachedCalc cc _) calc = atomically $ do
  m <- readTVar cc
  writeTVar cc $ HashMap.insert k (Left calc) m

-- | Store an already-computed value.
set :: Hashable k => k -> v -> CachedCalc k v -> IO ()
set k v cc = setCalc k cc (return v)

-- | Adds a calculation on top of an existing calculation.
-- First arg passed into `f` is Nothing if key doesn't yet exist.
modifyCalc :: Hashable k => (Maybe v -> IO v) -> k -> CachedCalc k v -> IO ()
modifyCalc f k (CachedCalc cc _) = do
  atomically $ do
    m <- readTVar cc
    case HashMap.lookup k m of
      Nothing -> writeTVar cc $ HashMap.insert k (Left $ f Nothing) m
      Just (Left action) -> do
        let action' = action >>= f . Just
        writeTVar cc $ HashMap.insert k (Left action') m
      Just (Right tmv) -> do
        let action = do
              result <- atomically (readTMVar tmv)
              either Ex.throwIO (f . Just) result
        writeTVar cc $ HashMap.insert k (Left action) m

get_ :: CachedCalc () v -> IO v
get_ = fmap fromJust . get ()

-- | Retrieves the cached calc. Returns Nothing if the key cannot be found
-- and no defaultCalc is set. If a defaultCalc is set, unknown keys are
-- computed on demand using it.
--
-- Uses 'mask' to ensure the TMVar is always filled even if the computing
-- thread receives an async exception (e.g. from forConcurrently cancellation).
-- Without this, the TMVar can be left empty, causing all waiters to deadlock
-- with "thread blocked indefinitely in an MVar/STM operation" and losing
-- the original exception.
get :: Hashable k => k -> CachedCalc k v -> IO (Maybe v)
get k (CachedCalc cc dc) = Ex.mask $ \restore -> do
  mer <- atomically $ do
    m <- readTVar cc
    case HashMap.lookup k m of
      Nothing -> readTVar dc >>= \case
        Nothing -> return Nothing
        Just f -> do
          tmv <- newEmptyTMVar
          writeTVar cc $ HashMap.insert k (Right tmv) m
          return . Just $ Left (f k, tmv)
      Just (Left calc) -> do
        tmv <- newEmptyTMVar
        writeTVar cc $ HashMap.insert k (Right tmv) m
        return . Just $ Left (calc, tmv)
      Just (Right tmv) -> return . Just $ Right tmv
  case mer of
    Nothing -> return Nothing
    Just (Right tmv) -> do
      result <- restore $ atomically $ readTMVar tmv
      case result of
        Left ex -> do
          hPutStrLn stderr $ "CachedCalc computation failed (re-raised): " <> Ex.displayException ex
          Ex.throwIO ex
        Right v -> return $ Just v
    Just (Left (calc, tmv)) -> do
      result <- Ex.try (restore calc)
      atomically $ putTMVar tmv result
      case result of
        Left (ex :: Ex.SomeException) -> do
          hPutStrLn stderr $ "CachedCalc computation failed: " <> Ex.displayException ex
          Ex.throwIO ex
        Right v -> return $ Just v

-- | Like `get`, but takes a computation and inserts a new entry if the key
-- doesn't exist. Guarantees compute-once semantics.
getOrCompute :: Hashable k => k -> IO v -> CachedCalc k v -> IO v
getOrCompute k calc cc@(CachedCalc cctv _) = do
  atomically $ do
    m <- readTVar cctv
    case HashMap.lookup k m of
      Just _ -> return ()
      Nothing -> writeTVar cctv $ HashMap.insert k (Left calc) m
  fromJust <$> get k cc

-- | Completely calcs current state of CachedCalc into reified map
getSnapshot :: Hashable k => CachedCalc k v -> IO (HashMap k v)
getSnapshot fullCc@(CachedCalc cc _) = do
  keys <- HashMap.keys <$> readTVarIO cc
  fmap (HashMap.fromList . catMaybes) . mapConcurrently getVal $ keys
  where
    getVal k = fmap (k,) <$> get k fullCc
