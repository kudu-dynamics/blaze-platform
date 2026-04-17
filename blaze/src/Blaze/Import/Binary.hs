module Blaze.Import.Binary where

import Blaze.Prelude

-- | Which IR stage(s) to render in backend inspection output.
--
-- Every backend is assumed to have two IR levels: a "low" stage (raw pcode
-- for Ghidra, LLIL for Binja) and a "high" stage (decompiled/high pcode for
-- Ghidra, MLIL for Binja). Tools that dump IR accept this to pick which
-- stage(s) to show.
data Stage = StageLow | StageHigh | StageBoth
  deriving (Eq, Show, Generic)

-- | User-facing identity of a binary backend — the backend's display name
-- and the names it uses for its low and high IR stages. Consumed by tool
-- code that builds output strings so labels aren't hardcoded to one backend.
data BackendDescriptor = BackendDescriptor
  { backendName :: Text
  , lowIrName :: Text
  , highIrName :: Text
  } deriving (Eq, Show, Generic)

-- | Generic placeholder descriptor used as the class default so backends
-- that haven't been migrated still compile. Real backends should override.
unknownBackendDescriptor :: BackendDescriptor
unknownBackendDescriptor = BackendDescriptor
  { backendName = "Unknown"
  , lowIrName = "low IR"
  , highIrName = "high IR"
  }

class BinaryImporter a where
  -- | This opens a binary or an importer db (like `bndb` or `gzf`)
  openBinary :: FilePath -> IO (Either Text a)
  -- | Action which is __required__ to be run before exiting the current
  -- process. It's a good idea to add this to the end of your @main@ action for
  -- any importer types that have been used.
  shutdown :: IO ()
  -- | Saves the importer to a db
  -- The FilePath that is returned is the full path to the db.
  -- The FilePath arg might or might not already include a db extension,
  -- so be sure to check before adding an extension.
  saveToDb :: FilePath -> a -> IO (Either Text FilePath)
  rebaseBinary :: a -> Address -> IO a
  getBase :: a -> IO Address
  getStart :: a -> IO Address
  getEnd :: a -> IO Address
  getOriginalBinaryPath :: a -> IO FilePath
  -- | Returns a map of addresses to their string contents from the binary.
  getStringsMap :: a -> IO (HashMap Address Text)
  -- | Backend display name and IR stage names. See 'BackendDescriptor'.
  describeBackend :: Proxy a -> BackendDescriptor
  describeBackend _ = unknownBackendDescriptor
  -- | Inspect the raw instruction and IR at a given address. The 'Stage'
  -- argument picks which IR level(s) to include in the rendered output.
  -- Returns Nothing if the address doesn't contain an instruction.
  inspectAddress :: a -> Address -> Stage -> IO (Maybe Text)
  inspectAddress _ _ _ = return Nothing
  -- | Dump the low and/or high IR for an entire function (identified by
  -- its entry address), optionally restricted to a contiguous address
  -- range. Returns a rendered text block or an error message if the
  -- function can't be dumped.
  dumpLift :: a -> Address -> Stage -> Maybe (Address, Address) -> IO (Either Text Text)
  dumpLift _ _ _ _ = return $ Left "dump_lift not supported by this backend"
  -- | Look up a global symbol by name and return its address.
  lookupGlobalSymbol :: a -> Text -> IO (Maybe Address)
  lookupGlobalSymbol _ _ = return Nothing
