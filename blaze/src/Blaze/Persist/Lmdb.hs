module Blaze.Persist.Lmdb
  ( LmdbEnv
  , LmdbStore
  , openEnv
  , openStore
  , lmdbGet
  , lmdbPut
  , lmdbGetAll
  , closeEnv
  ) where

import Blaze.Prelude hiding (get)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Database.LMDB.Raw
  ( MDB_env
  , MDB_dbi
  , MDB_val(..)
  , MDB_cursor_op(..)
  , mdb_env_create
  , mdb_env_open
  , mdb_env_close
  , mdb_env_set_mapsize
  , mdb_env_set_maxdbs
  , mdb_txn_begin
  , mdb_txn_commit
  , mdb_txn_abort
  , mdb_dbi_open
  , mdb_get
  , mdb_put
  , mdb_cursor_open
  , mdb_cursor_get
  , mdb_cursor_close
  , MDB_EnvFlag(..)
  , MDB_DbFlag(..)
  , compileWriteFlags
  )
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Foreign.C.Types (CSize(..))
import qualified Control.Exception as Ex

-- | An LMDB environment wrapping a single database file.
-- Write operations are funneled through a dedicated bound OS thread
-- because LMDB requires write transactions on bound threads.
data LmdbEnv = LmdbEnv
  { _envHandle :: !MDB_env
  , _envWriter :: !(IO () -> IO ())  -- run an action on the bound writer thread
  }

-- | A named sub-database within an LMDB environment.
data LmdbStore = LmdbStore
  { _storeEnv :: !LmdbEnv
  , _storeDbi :: !MDB_dbi
  } deriving (Generic)

-- | Default map size: 1 TB. This is a virtual address space reservation,
-- not actual disk/RAM usage — the file is sparse and only grows as data is added.
defaultMapSize :: Int
defaultMapSize = 1024 * 1024 * 1024 * 1024

-- | Maximum number of named sub-databases.
defaultMaxDbs :: Int
defaultMaxDbs = 16

-- | Open (or create) an LMDB environment at the given file path.
-- Uses MDB_NOSUBDIR so the path is treated as a file, not a directory
-- (e.g., @something.gzf.flintdb@). A lock file is created alongside it.
-- Spawns a dedicated bound OS thread for serialized write operations.
openEnv :: FilePath -> IO LmdbEnv
openEnv path = do
  env <- mdb_env_create
  mdb_env_set_mapsize env defaultMapSize
  mdb_env_set_maxdbs env defaultMaxDbs
  mdb_env_open env path [MDB_NOSUBDIR]
  LmdbEnv env <$> spawnBoundWriter

-- | Open (or create) a named sub-database within an environment.
-- Must be called before any concurrent access to this sub-database.
openStore :: LmdbEnv -> Text -> IO LmdbStore
openStore lenv@(LmdbEnv env writer) name = do
  dbi <- runOnWriter writer $ do
    txn <- mdb_txn_begin env Nothing False
    dbi <- mdb_dbi_open txn (Just $ cs name) [MDB_CREATE]
    mdb_txn_commit txn
    return dbi
  return $ LmdbStore lenv dbi

-- | Look up a key in the store. Returns Nothing if the key is not found.
-- Read-only — does not require the bound writer thread.
lmdbGet :: LmdbStore -> ByteString -> IO (Maybe ByteString)
lmdbGet (LmdbStore (LmdbEnv env _) dbi) key = do
  txn <- mdb_txn_begin env Nothing True  -- read-only
  result <- withMdbVal key $ \kval ->
    mdb_get txn dbi kval
  mbs <- case result of
    Nothing -> return Nothing
    Just val -> Just <$> mdbValToBS val
  mdb_txn_abort txn
  return mbs

-- | Store a key-value pair. Overwrites any existing value for the key.
-- Runs on the dedicated bound writer thread.
lmdbPut :: LmdbStore -> ByteString -> ByteString -> IO ()
lmdbPut (LmdbStore (LmdbEnv env writer) dbi) key val =
  runOnWriter writer $ do
    txn <- mdb_txn_begin env Nothing False  -- read-write
    withMdbVal key $ \kval ->
      withMdbVal val $ \vval ->
        void $ mdb_put (compileWriteFlags []) txn dbi kval vval
    mdb_txn_commit txn

-- | Enumerate all key-value pairs in the store.
-- Read-only — does not require the bound writer thread.
lmdbGetAll :: LmdbStore -> IO [(ByteString, ByteString)]
lmdbGetAll (LmdbStore (LmdbEnv env _) dbi) = do
  txn <- mdb_txn_begin env Nothing True  -- read-only
  cursor <- mdb_cursor_open txn dbi
  entries <- alloca $ \kptr -> alloca $ \vptr -> do
    let go op acc = do
          found <- mdb_cursor_get op cursor kptr vptr
          if found
            then do
              kval <- peek kptr
              vval <- peek vptr
              kbs <- mdbValToBS kval
              vbs <- mdbValToBS vval
              go MDB_NEXT ((kbs, vbs) : acc)
            else return (reverse acc)
    go MDB_FIRST []
  mdb_cursor_close cursor
  mdb_txn_abort txn
  return entries

-- | Close the LMDB environment and release resources.
closeEnv :: LmdbEnv -> IO ()
closeEnv (LmdbEnv env _) = mdb_env_close env

-- Writer thread infrastructure

-- | Spawn a dedicated bound OS thread that processes write actions serially.
-- Returns a function that submits an action to the writer and blocks until done.
spawnBoundWriter :: IO (IO () -> IO ())
spawnBoundWriter = do
  -- Channel: (action to run, MVar to signal completion/error)
  chan <- newChan
  _ <- forkOS $ forever $ do
    (action, resultMVar) <- readChan chan
    result <- Ex.try action
    putMVar resultMVar result
  return $ \action -> do
    resultMVar <- newEmptyMVar
    writeChan chan (action, resultMVar)
    result <- takeMVar resultMVar
    case result of
      Left (ex :: Ex.SomeException) -> Ex.throwIO ex
      Right () -> return ()

-- | Run an action on the writer thread and return its result.
-- The action is wrapped to pass the result back via MVar.
runOnWriter :: (IO () -> IO ()) -> IO a -> IO a
runOnWriter writer action = do
  resultMVar <- newEmptyMVar
  writer $ do
    r <- action
    putMVar resultMVar r
  takeMVar resultMVar

-- Internal helpers

-- | Run an action with a ByteString converted to an MDB_val.
withMdbVal :: ByteString -> (MDB_val -> IO a) -> IO a
withMdbVal bs f =
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    f $ MDB_val (fromIntegral len) (castPtr ptr)

-- | Copy an MDB_val into a Haskell ByteString.
-- The MDB_val points into LMDB's memory-mapped pages, so we must copy
-- before the transaction ends.
mdbValToBS :: MDB_val -> IO ByteString
mdbValToBS (MDB_val (CSize len) ptr) =
  BS.packCStringLen (castPtr ptr, fromIntegral len)
