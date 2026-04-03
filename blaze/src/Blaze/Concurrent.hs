module Blaze.Concurrent
  ( WorkerPool
  , newWorkerPool
  , newAutoWorkerPool
  , poolWorkerCount
  , withWorker
  , pooledForConcurrently
  , pooledForConcurrently_
  , pooledMapConcurrently
  , pooledMapConcurrently_
  ) where

import Blaze.Prelude hiding (get)

import Control.Concurrent.Async (forConcurrently, forConcurrently_)
import GHC.Conc (getNumProcessors)


-- | A bounded worker pool backed by a counting semaphore.
-- Multiple call sites can share the same pool — they all compete
-- for the same set of worker slots.
data WorkerPool = WorkerPool
  { _poolSem :: !QSem
  , _poolSize :: !Int
  }

-- | Create a pool with an explicit worker count.
newWorkerPool :: Int -> IO WorkerPool
newWorkerPool n = do
  let size = max 1 n
  sem <- newQSem size
  return $ WorkerPool sem size

-- | Create a pool sized to @(CPUs - 2)@, minimum 1.
-- Leaves headroom for GC and the OS.
newAutoWorkerPool :: IO WorkerPool
newAutoWorkerPool = do
  cpus <- getNumProcessors
  newWorkerPool (cpus - 2)

-- | How many worker slots this pool has.
poolWorkerCount :: WorkerPool -> Int
poolWorkerCount = _poolSize

-- | Run an action using one worker slot from the pool.
-- Blocks until a slot is available, then releases it when done
-- (even if the action throws).
withWorker :: WorkerPool -> IO a -> IO a
withWorker (WorkerPool sem _) = bracket_ (waitQSem sem) (signalQSem sem)

-- | Like 'forConcurrently' but bounded by the pool.
-- Spawns all tasks as green threads, but only @poolSize@ run at a time.
pooledForConcurrently :: Traversable t => WorkerPool -> t a -> (a -> IO b) -> IO (t b)
pooledForConcurrently pool xs f =
  forConcurrently xs $ \x -> withWorker pool (f x)

-- | Like 'forConcurrently_' but bounded by the pool.
pooledForConcurrently_ :: Foldable t => WorkerPool -> t a -> (a -> IO b) -> IO ()
pooledForConcurrently_ pool xs f =
  forConcurrently_ xs $ \x -> withWorker pool (f x)

-- | Like 'mapConcurrently' but bounded by the pool.
pooledMapConcurrently :: Traversable t => WorkerPool -> (a -> IO b) -> t a -> IO (t b)
pooledMapConcurrently pool f =
  pooledForConcurrently pool `flip` f

-- | Like 'mapConcurrently_' but bounded by the pool.
pooledMapConcurrently_ :: Foldable t => WorkerPool -> (a -> IO b) -> t a -> IO ()
pooledMapConcurrently_ pool f =
  pooledForConcurrently_ pool `flip` f
