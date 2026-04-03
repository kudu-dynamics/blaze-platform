module Blaze.Types.PersistentCalc where

import Blaze.Prelude hiding (get, show)
import Blaze.Types.CachedCalc (CachedCalc)
import qualified Blaze.Types.CachedCalc as CC
import qualified Blaze.Persist.Lmdb as Lmdb
import Blaze.Persist.Lmdb (LmdbStore)

import Prelude (show)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Serialize as Serialize
import qualified Control.Exception as Ex


-- | Serialization layer for persisting a CachedCalc's entries to LMDB.
data PersistLayer k v = PersistLayer
  { lmdbStore :: LmdbStore
  , encodeKey :: k -> ByteString
  , decodeKey :: ByteString -> Maybe k
  , encodeVal :: v -> ByteString
  , decodeVal :: ByteString -> Maybe v
  }

-- | A CachedCalc with an optional LMDB persistence layer.
-- When the persist layer is present, computations check the DB before
-- running the expensive calc, and store results to the DB after computing.
-- When the persist layer is Nothing, this behaves identically to CachedCalc.
data PersistentCalc k v = PersistentCalc
  { cachedCalc :: CachedCalc k v
  , persistLayer :: Maybe (PersistLayer k v)
  } deriving (Generic)

instance Show (PersistentCalc k v) where
  show _ = "<PersistentCalc>"

-- | Create a new PersistentCalc, optionally backed by LMDB.
create :: Maybe (PersistLayer k v) -> STM (PersistentCalc k v)
create mpl = do
  cc <- CC.create
  return $ PersistentCalc cc mpl

-- | Register a lazy computation for a specific key.
-- If a persist layer is present, the computation is wrapped to:
--   1. Check the DB first
--   2. If not in DB, run the original computation and persist the result
setCalc :: Hashable k => k -> PersistentCalc k v -> IO v -> IO ()
setCalc k (PersistentCalc cc mpl) calc =
  CC.setCalc k cc $ wrapCalcForKey mpl k calc

-- | Set a default computation for unknown keys, with DB-awareness.
setDefault :: (k -> IO v) -> PersistentCalc k v -> IO ()
setDefault f (PersistentCalc cc mpl) =
  CC.setDefault (\k -> wrapCalcForKey mpl k (f k)) cc

-- | Store an already-computed value in both the cache and DB.
set :: Hashable k => k -> v -> PersistentCalc k v -> IO ()
set k v (PersistentCalc cc mpl) = do
  CC.set k v cc
  persistValue mpl k v

-- | Retrieve a cached/persisted value. The wrapped calc functions handle
-- the DB lookup transparently.
get :: Hashable k => k -> PersistentCalc k v -> IO (Maybe v)
get k (PersistentCalc cc _) = CC.get k cc

-- | Convenience: get with () key.
get_ :: PersistentCalc () v -> IO v
get_ = fmap fromJust . get ()

-- | Get or compute with compute-once semantics.
getOrCompute :: Hashable k => k -> IO v -> PersistentCalc k v -> IO v
getOrCompute k calc (PersistentCalc cc mpl) = do
  -- Insert a DB-aware calc if key doesn't exist yet, then get
  CC.getOrCompute k (wrapCalcForKey mpl k calc) cc
  -- CC.getOrCompute only inserts if key is missing, then calls CC.get
  -- which will run the wrapped calc. But if the key already had a calc
  -- registered via setCalc, that one takes priority. The wrapping is
  -- consistent either way.

-- | Layer a new computation on top of an existing one.
modifyCalc :: Hashable k => (Maybe v -> IO v) -> k -> PersistentCalc k v -> IO ()
modifyCalc f k (PersistentCalc cc mpl) =
  CC.modifyCalc (\mv -> do
    v <- f mv
    persistValue mpl k v
    return v
  ) k cc

-- | Get all keys — union of in-memory cache keys and DB keys.
getKeys :: Hashable k => PersistentCalc k v -> IO [k]
getKeys (PersistentCalc cc mpl) = do
  memKeys <- CC.getKeys cc
  case mpl of
    Nothing -> return memKeys
    Just pl -> do
      dbEntries <- lmdbGetAllKeys pl
      let memKeySet = HashSet.fromList memKeys
          newDbKeys = filter (not . (`HashSet.member` memKeySet)) dbEntries
      return $ memKeys <> newDbKeys

-- | Materialize all entries from both cache and DB.
getSnapshot :: Hashable k => PersistentCalc k v -> IO (HashMap k v)
getSnapshot (PersistentCalc cc mpl) = do
  -- First get the in-memory snapshot
  memSnap <- CC.getSnapshot cc
  case mpl of
    Nothing -> return memSnap
    Just pl -> do
      -- Load any DB entries not already in memory
      dbEntries <- lmdbGetAllEntries pl
      let dbMap = HashMap.fromList dbEntries
      -- Memory takes precedence over DB
      return $ HashMap.union memSnap dbMap

-- Internal helpers

-- | Wrap a computation to check LMDB before running it.
wrapCalcForKey :: Maybe (PersistLayer k v) -> k -> IO v -> IO v
wrapCalcForKey Nothing _k calc = calc
wrapCalcForKey (Just pl) k calc = do
  let keyBs = encodeKey pl k
  mVal <- lmdbGetSafe (lmdbStore pl) keyBs
  case mVal >>= decodeVal pl of
    Just v -> return v
    Nothing -> do
      v <- calc
      lmdbPutSafe (lmdbStore pl) keyBs (encodeVal pl v)
      return v

-- | Persist a value to DB (no-op if no persist layer).
persistValue :: Maybe (PersistLayer k v) -> k -> v -> IO ()
persistValue Nothing _k _v = return ()
persistValue (Just pl) k v =
  lmdbPutSafe (lmdbStore pl) (encodeKey pl k) (encodeVal pl v)

-- | Get all decoded keys from the DB.
lmdbGetAllKeys :: PersistLayer k v -> IO [k]
lmdbGetAllKeys pl = do
  entries <- lmdbGetAllSafe (lmdbStore pl)
  return $ mapMaybe (decodeKey pl . fst) entries

-- | Get all decoded key-value pairs from the DB.
lmdbGetAllEntries :: PersistLayer k v -> IO [(k, v)]
lmdbGetAllEntries pl = do
  entries <- lmdbGetAllSafe (lmdbStore pl)
  return $ mapMaybe (\(kbs, vbs) -> (,) <$> decodeKey pl kbs <*> decodeVal pl vbs) entries

-- | Safe LMDB get that catches exceptions and returns Nothing.
lmdbGetSafe :: LmdbStore -> ByteString -> IO (Maybe ByteString)
lmdbGetSafe store key = do
  result <- try $ Lmdb.lmdbGet store key
  case result of
    Left (ex :: Ex.SomeException) -> do
      hPutStrLn stderr $ "PersistentCalc: LMDB get failed: " <> Ex.displayException ex
      return Nothing
    Right v -> return v

-- | Safe LMDB put that logs but doesn't throw on failure.
lmdbPutSafe :: LmdbStore -> ByteString -> ByteString -> IO ()
lmdbPutSafe store key val = do
  result <- try $ Lmdb.lmdbPut store key val
  case result of
    Left (ex :: Ex.SomeException) ->
      hPutStrLn stderr $ "PersistentCalc: LMDB put failed: " <> Ex.displayException ex
    Right () -> return ()

-- | Safe LMDB getAll that returns empty on failure.
lmdbGetAllSafe :: LmdbStore -> IO [(ByteString, ByteString)]
lmdbGetAllSafe store = do
  result <- try $ Lmdb.lmdbGetAll store
  case result of
    Left (ex :: Ex.SomeException) -> do
      hPutStrLn stderr $ "PersistentCalc: LMDB getAll failed: " <> Ex.displayException ex
      return []
    Right entries -> return entries

-- | Helper to build a PersistLayer using cereal binary serialization.
mkPersistLayer
  :: (Serialize v, Serialize k)
  => LmdbStore
  -> PersistLayer k v
mkPersistLayer store = PersistLayer
  { lmdbStore = store
  , encodeKey = Serialize.encode
  , decodeKey = either (const Nothing) Just . Serialize.decode
  , encodeVal = Serialize.encode
  , decodeVal = either (const Nothing) Just . Serialize.decode
  }
