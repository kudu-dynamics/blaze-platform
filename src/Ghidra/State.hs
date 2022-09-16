module Ghidra.State where

import Ghidra.Prelude hiding (force, get)

import qualified Language.Java as Java
import Ghidra.Util (maybeNullCall, suppressOut)
import qualified Ghidra.Types as J
import qualified Data.BinaryAnalysis as BA
import qualified Foreign.JNI as JNI


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

getFlatDecompilerAPI :: GhidraState -> IO J.FlatDecompilerAPI
getFlatDecompilerAPI = return . view #flatDecompilerAPI

getLang :: Text -> IO J.Language
getLang lang = do
  langProvider :: J.SleighLanguageProvider <- Java.new >>= JNI.newGlobalRef
  maybeNullCall (Java.reflect lang >>= Java.new) >>= \case
    Nothing -> error $ "Could not find lang: " <> cs lang
    Just (langId :: J.LanguageID) -> do
      Java.call langProvider "getLanguage" langId >>= JNI.newGlobalRef

getCSpec :: J.Language -> Maybe Text -> IO J.CompilerSpec
getCSpec lang Nothing = Java.call lang "getDefaultCompilerSpec"
getCSpec lang (Just compilerName) = do
  compSpecId :: J.CompilerSpecID <- Java.reflect compilerName >>= Java.new >>= JNI.newGlobalRef
  Java.call lang "getCompilerSpecByID" compSpecId >>= JNI.newGlobalRef

hasBeenAnalyzed :: GhidraState -> IO Bool
hasBeenAnalyzed gs = do
  programInfoField :: J.String <- Java.getStaticField "ghidra.program.model.listing.Program" "PROGRAM_INFO" >>= JNI.newGlobalRef
  analyzedField :: J.String <- Java.getStaticField "ghidra.program.model.listing.Program" "ANALYZED" >>= Java.reify >>= JNI.newGlobalRef
  prgOptions :: J.Options <- Java.call (gs ^. #program) "getOptions" programInfoField >>= JNI.newGlobalRef
  Java.call prgOptions "getBoolean" analyzedField False

-- | Opens binary as Ghidra database.
-- Throws error if file not found.
-- TODO: make this return (Either err GhidraState)
openDatabase' :: OpenDatabaseOptions -> FilePath -> IO GhidraState
openDatabase' opts fp = do
  config :: J.HeadlessGhidraApplicationConfiguration <- Java.new >>= JNI.newGlobalRef
  layout :: J.GhidraJarApplicationLayout <- Java.new >>= JNI.newGlobalRef
  _ :: () <- bool identity suppressOut (opts ^. #quiet) $ do
    Java.callStatic "ghidra.framework.Application" "isInitialized" >>= \case
      True -> return ()
      False -> Java.callStatic "ghidra.framework.Application" "initializeApplication" (coerce layout :: J.ApplicationLayout) (coerce config :: J.ApplicationConfiguration)
  consumer :: J.Object <- Java.new >>= JNI.newGlobalRef
  messageLog :: J.MessageLog <- Java.new >>= JNI.newGlobalRef
  tm :: J.TaskMonitor <- Java.getStaticField "ghidra.util.task.TaskMonitor" "DUMMY" >>= JNI.newGlobalRef
  file :: J.File <- Java.reflect (cs fp :: Text) >>= Java.new >>= JNI.newGlobalRef
  prg :: J.Program <- case opts ^. #language of
    Nothing -> Java.callStatic "ghidra.app.util.importer.AutoImporter" "importByUsingBestGuess" file (Java.jnull :: J.DomainFolder) consumer messageLog tm >>= JNI.newGlobalRef
    Just lang -> do
      lang' <- getLang lang
      cspec <- getCSpec lang' $ opts ^. #compiler
      Java.callStatic "ghidra.app.util.importer.AutoImporter" "importByLookingForLcs" file (Java.jnull :: J.DomainFolder) lang' cspec consumer messageLog tm >>= JNI.newGlobalRef
  flatApi :: J.FlatProgramAPI <- Java.new prg >>= JNI.newGlobalRef
  flatDecApi :: J.FlatDecompilerAPI <- Java.new flatApi >>= JNI.newGlobalRef
  return $ GhidraState tm (coerce prg :: J.ProgramDB) flatApi flatDecApi

openDatabase :: FilePath -> IO GhidraState
openDatabase = openDatabase' defaultOpenDatabaseOptions

data AnalyzeOptions = AnalyzeOptions
  { force :: Bool
  , quiet :: Bool
  } deriving (Eq, Ord, Show, Generic)

defaultAnalyzeOptions :: AnalyzeOptions
defaultAnalyzeOptions = AnalyzeOptions False True


analyze' :: AnalyzeOptions -> GhidraState -> IO GhidraState
analyze' opts gs = do
  alreadyAnalyzed <- hasBeenAnalyzed gs
  if not alreadyAnalyzed || (opts ^. #force)
    then doAnalysis
    else return gs
  where
    doAnalysis = bool identity suppressOut (opts ^. #quiet) $ do
      let prg = gs ^. #program
      txId :: Int32 <- Java.call prg "startTransaction" =<< Java.reflect ("Analysis" :: Text)
      _ :: () <- Java.call (gs ^. #flatProgramAPI) "analyzeAll" (coerce prg :: J.Program)
      _ :: () <- Java.callStatic "ghidra.program.util.GhidraProgramUtilities" "setAnalyzedFlag" (coerce prg :: J.Program) True
      _ :: () <- Java.call prg "endTransaction" txId True
      return gs

analyze :: GhidraState -> IO GhidraState
analyze = analyze' defaultAnalyzeOptions



  
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


-- getTaskMonitor :: GhidraState -> IO J.TaskMonitor
-- getTaskMonitor (GhidraState gs) = do
--   k <- keyword "task-monitor"
--   coerce <$> get gs k

-- getFlatDecompilerAPI :: GhidraState -> IO J.FlatDecompilerAPI
-- getFlatDecompilerAPI (GhidraState gs) = do
--   k <- keyword "flat-dec-api"
--   coerce <$> get gs k






getListing :: GhidraState -> IO J.Listing
getListing gs = do
  prg <- getProgram gs
  Java.call prg "getListing" 

-- | Adds address to image base.
-- Only use this with PIE binaries.
mkAddressBased :: GhidraState -> BA.Address -> IO J.Address
mkAddressBased gs addr = do
  prg <- getProgram gs
  baseAddr :: J.Address <- Java.call prg "getImageBase" >>= JNI.newGlobalRef
  Java.call baseAddr "add" (fromIntegral addr :: Int64)

-- | Makes a new address
mkAddress :: GhidraState -> BA.Address -> IO J.Address
mkAddress gs addr = do
  prg <- getProgram gs
  baseAddr :: J.Address <- Java.call prg "getImageBase" >>= JNI.newGlobalRef
  Java.call baseAddr "getNewAddress" (fromIntegral addr :: Int64)
