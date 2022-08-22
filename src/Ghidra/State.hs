module Ghidra.State where

import Ghidra.Prelude hiding (force, get)

import Language.Clojure
import Foreign.JNI.Types (JObject)
import qualified Data.Text as Text
import qualified Language.Java as Java
import System.IO.Memoize (once)
import Ghidra.Program (Program)
import Ghidra.Util (convertOpt)
import Ghidra.Types (Address, FlatDecompilerAPI, TaskMonitor)
import qualified Data.BinaryAnalysis as BA

requireModule :: IO ()
requireModule = unsafePerformIO . once $ do
  _ <- readEval "(require (quote [ghidra-clojure.state]))"
  return ()

newtype GhidraState = GhidraState { unGhidraState :: JObject }
  deriving (Eq, Show, Generic)

data DatabaseOptions = DatabaseOptions
  { compiler :: Maybe Text
  , language :: Maybe Text
  , quiet :: Maybe Bool
  } deriving (Eq, Ord, Show, Generic)

defaultDatabaseOptions :: DatabaseOptions
defaultDatabaseOptions = DatabaseOptions Nothing Nothing Nothing

prepDatabaseOpts :: DatabaseOptions -> IO [JObject]
prepDatabaseOpts opts = do
  a <- convertOpt "compiler" $ opts ^. #compiler
  b <- convertOpt "language" $ opts ^. #language
  c <- convertOpt "quiet" $ opts ^. #quiet
  return $ a <> b <> c

openDatabase' :: Maybe DatabaseOptions -> FilePath -> IO GhidraState
openDatabase' mOpts fp = do
  requireModule
  dbPath <- Java.reflect $ Text.pack fp
  openDb <- varQual "ghidra-clojure.state" "open-database"
  case mOpts of
    Nothing -> GhidraState <$> invoke openDb (coerce dbPath :: JObject)
    Just opts -> do
      cljOpts <- prepDatabaseOpts opts
      GhidraState <$> applyInvoke openDb ((coerce dbPath :: JObject) : cljOpts)

openDatabase :: FilePath -> IO GhidraState
openDatabase = openDatabase' Nothing

data AnalyzeOptions = AnalyzeOptions
  { force :: Maybe Bool
  , quiet :: Maybe Bool
  } deriving (Eq, Ord, Show, Generic)

prepAnalyzeOpts :: AnalyzeOptions -> IO [JObject]
prepAnalyzeOpts opts = do
  a <- convertOpt "force" $ opts ^. #force
  b <- convertOpt "quiet" $ opts ^. #quiet
  return $ a <> b

analyze' :: Maybe AnalyzeOptions -> GhidraState -> IO GhidraState
analyze' mOpts (GhidraState gs) = do
  requireModule
  let analyzeFn = unsafeDupablePerformIO $ varQual "ghidra-clojure.state" "analyze"
  GhidraState <$> case mOpts of
    Nothing -> invoke analyzeFn gs
    Just opts -> applyInvoke analyzeFn . (gs:) =<< prepAnalyzeOpts opts

analyze :: GhidraState -> IO GhidraState
analyze = analyze' Nothing

getProgram :: GhidraState -> IO Program
getProgram (GhidraState gs) = do
  k <- keyword "program"
  coerce <$> get gs k

getFlatDecompilerAPI :: GhidraState -> IO FlatDecompilerAPI
getFlatDecompilerAPI (GhidraState gs) = do
  k <- keyword "flat-dec-api"
  coerce <$> get gs k

getTaskMonitor :: GhidraState -> IO TaskMonitor
getTaskMonitor (GhidraState gs) = do
  k <- keyword "task-monitor"
  coerce <$> get gs k

-- | Adds address to image base.
-- Only use this with PIE binaries.
mkAddressBased :: GhidraState -> BA.Address -> IO Address
mkAddressBased gs addr = do
  prg <- getProgram gs
  baseAddr :: Address <- Java.call prg "getImageBase"
  Java.call baseAddr "add" (fromIntegral addr :: Int64)

-- | Makes a new address
mkAddress :: GhidraState -> BA.Address -> IO Address
mkAddress gs addr = do
  prg <- getProgram gs
  baseAddr :: Address <- Java.call prg "getImageBase"
  Java.call baseAddr "getNewAddress" (fromIntegral addr :: Int64)
