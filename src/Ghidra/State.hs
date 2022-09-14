module Ghidra.State where

import Ghidra.Prelude hiding (force, get)

import Language.Clojure
import Foreign.JNI.Types (JObject)
import qualified Data.Text as Text
import qualified Language.Java as Java
import System.IO.Memoize (once)
import Ghidra.Util (convertOpt, maybeNullCall, suppressOut)
import qualified Ghidra.Types as J
import qualified Data.BinaryAnalysis as BA
import qualified Foreign.JNI as JNI


requireModule :: IO ()
requireModule = unsafePerformIO . once $ do
  _ <- readEval "(require (quote [ghidra-clojure.state]))"
  return ()

data GhidraState = GhidraState
  { taskMonitor :: J.TaskMonitor
  , program :: J.ProgramDB
  , flatProgramAPI :: J.FlatProgramAPI
  , flatDecompilerAPI :: J.FlatDecompilerAPI
  } deriving (Eq, Show, Generic)

data OpenDatabaseOptions = OpenDatabaseOptions
  { compiler :: Maybe Text
  , language :: Maybe Text
  , quiet :: Bool
  } deriving (Eq, Ord, Show, Generic)

defaultOpenDatabaseOptions :: OpenDatabaseOptions
defaultOpenDatabaseOptions = OpenDatabaseOptions Nothing Nothing True

getProgram :: GhidraState -> IO J.ProgramDB
getProgram = return . view #program

getTaskMonitor :: GhidraState -> IO J.TaskMonitor
getTaskMonitor = return . view #taskMonitor

getLang :: Text -> IO J.Language
getLang lang = do
  langProvider :: J.SleighLanguageProvider <- Java.new
  maybeNullCall (Java.reflect lang >>= Java.new) >>= \case
    Nothing -> error $ "Could not find lang: " <> cs lang
    Just (langId :: J.LanguageID) -> do
      Java.call langProvider "getLanguage" langId

getCSpec :: J.Language -> Maybe Text -> IO J.CompilerSpec
getCSpec lang Nothing = Java.call lang "getDefaultCompilerSpec"
getCSpec lang (Just compilerName) = do
  compSpecId :: J.CompilerSpecID <- Java.reflect compilerName >>= Java.new 
  Java.call lang "getCompilerSpecByID" compSpecId

-- hasBeenAnalyzed :: GhidraState -> IO Bool
-- hasBeenAnalyzed gs = do
--   programInfoField <- Java.getStaticField "ghidra.program.database.Program" "PROGRAM_INFO"
--   analyzedField <- programInfoField <- Java.getStaticField "ghidra.program.database.Program" "ANALYZED"
--   prgInfo <- Java.call

-- | Opens binary as Ghidra database.
-- Throws error if file not found.
openDatabase' :: OpenDatabaseOptions -> FilePath -> IO GhidraState
openDatabase' opts fp = do
  config :: J.HeadlessGhidraApplicationConfiguration <- Java.new
  layout :: J.GhidraJarApplicationLayout <- Java.new
  _ :: () <- bool identity suppressOut (opts ^. #quiet)
    $ Java.callStatic "ghidra.framework.Application" "initializeApplication" layout config
  consumer :: J.Object <- Java.new
  messageLog :: J.MessageLog <- Java.new
  tm :: J.TaskMonitor <- Java.getStaticField "ghidra.util.task.TaskMonitor" "DUMMY"
  file :: J.File <- Java.reflect (cs fp :: Text) >>= Java.new
  prg :: J.ProgramDB <- case opts ^. #language of
    Nothing -> Java.callStatic "ghidra.app.util.importer.AutoImporter" "importByUsingBestGuess" file () consumer messageLog
    Just lang -> do
      Java.callStatic "ghidra.app.util.importer.AutoImporter" "importByUsingBestGuess" file () consumer messageLog tm
  flatApi :: J.FlatProgramAPI <- Java.new prg
  flatDecApi :: J.FlatDecompilerAPI <- Java.new flatApi
  return $ GhidraState tm prg flatApi flatDecApi

openDatabase :: FilePath -> IO GhidraState
openDatabase = openDatabase' defaultOpenDatabaseOptions

data AnalyzeOptions = AnalyzeOptions
  { force :: Bool
  , quiet :: Bool
  } deriving (Eq, Ord, Show, Generic)

defaultAnalyzeOptions :: AnalyzeOptions
defaultAnalyzeOptions = AnalyzeOptions False True

-- analyze' :: AnalyzeOptions -> IO GhidraState
-- analyze' opts = do
--   _ 

-- newtype GhidraState = GhidraState { unGhidraState :: JObject }
--   deriving (Eq, Show, Generic)

-- data DatabaseOptions = DatabaseOptions
--   { compiler :: Maybe Text
--   , language :: Maybe Text
--   , quiet :: Maybe Bool
--   } deriving (Eq, Ord, Show, Generic)

-- defaultDatabaseOptions :: DatabaseOptions
-- defaultDatabaseOptions = DatabaseOptions Nothing Nothing Nothing

-- prepDatabaseOpts :: DatabaseOptions -> IO [JObject]
-- prepDatabaseOpts opts = do
--   a <- convertOpt "compiler" $ opts ^. #compiler
--   b <- convertOpt "language" $ opts ^. #language
--   c <- convertOpt "quiet" $ opts ^. #quiet
--   return $ a <> b <> c

-- openDatabase' :: Maybe DatabaseOptions -> FilePath -> IO GhidraState
-- openDatabase' mOpts fp = do
--   requireModule
--   dbPath <- Java.reflect $ Text.pack fp
--   openDb <- varQual "ghidra-clojure.state" "open-database"
--   case mOpts of
--     Nothing -> GhidraState <$> invoke openDb (coerce dbPath :: JObject)
--     Just opts -> do
--       cljOpts <- prepDatabaseOpts opts
--       GhidraState <$> applyInvoke openDb ((coerce dbPath :: JObject) : cljOpts)

-- openDatabase :: FilePath -> IO GhidraState
-- openDatabase = openDatabase' Nothing

-- data AnalyzeOptions = AnalyzeOptions
--   { force :: Maybe Bool
--   , quiet :: Maybe Bool
--   } deriving (Eq, Ord, Show, Generic)

-- prepAnalyzeOpts :: AnalyzeOptions -> IO [JObject]
-- prepAnalyzeOpts opts = do
--   a <- convertOpt "force" $ opts ^. #force
--   b <- convertOpt "quiet" $ opts ^. #quiet
--   return $ a <> b

-- analyze' :: Maybe AnalyzeOptions -> GhidraState -> IO GhidraState
-- analyze' mOpts (GhidraState gs) = do
--   requireModule
--   let analyzeFn = unsafeDupablePerformIO $ varQual "ghidra-clojure.state" "analyze"
--   GhidraState <$> case mOpts of
--     Nothing -> invoke analyzeFn gs
--     Just opts -> applyInvoke analyzeFn . (gs:) =<< prepAnalyzeOpts opts

-- analyze :: GhidraState -> IO GhidraState
-- analyze = analyze' Nothing

-- getProgram :: GhidraState -> IO J.ProgramDB
-- getProgram (GhidraState gs) = do
--   k <- keyword "program"
--   coerce <$> get gs k

getListing :: GhidraState -> IO J.Listing
getListing gs = do
  prg <- getProgram gs
  Java.call prg "getListing" 

getFlatDecompilerAPI :: GhidraState -> IO J.FlatDecompilerAPI
getFlatDecompilerAPI = return . view #flatDecompilerAPI

-- getTaskMonitor :: GhidraState -> IO J.TaskMonitor
-- getTaskMonitor (GhidraState gs) = do
--   k <- keyword "task-monitor"
--   coerce <$> get gs k

-- -- | Adds address to image base.
-- -- Only use this with PIE binaries.
-- mkAddressBased :: GhidraState -> BA.Address -> IO J.Address
-- mkAddressBased gs addr = do
--   prg <- getProgram gs
--   baseAddr :: J.Address <- Java.call prg "getImageBase" >>= JNI.newGlobalRef
--   Java.call baseAddr "add" (fromIntegral addr :: Int64)

-- -- | Makes a new address
-- mkAddress :: GhidraState -> BA.Address -> IO J.Address
-- mkAddress gs addr = do
--   prg <- getProgram gs
--   baseAddr :: J.Address <- Java.call prg "getImageBase" >>= JNI.newGlobalRef
--   Java.call baseAddr "getNewAddress" (fromIntegral addr :: Int64)
