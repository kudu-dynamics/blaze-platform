module Ghidra.State where

import Ghidra.Prelude hiding (force, get)

import qualified Language.Java as Java
import Ghidra.Util (iteratorToList, maybeNullCall, suppressOut, tryJVM, getDomainObject)
import qualified Ghidra.Types as J
import Ghidra.Types.Internal (Ghidra, runIO)
import qualified Data.BinaryAnalysis as BA
import qualified Foreign.JNI as JNI
import qualified Data.Text as Text


data GhidraState = GhidraState
  { taskMonitor :: J.TaskMonitor
  , program :: J.ProgramDB
  , flatProgramAPI :: J.FlatProgramAPI
  , flatDecompilerAPI :: J.FlatDecompilerAPI
  } deriving (Eq, Ord, Show, Generic)

data OpenDatabaseOptions = OpenDatabaseOptions
  { compiler :: Maybe Text
  , language :: Maybe Text
  , quiet :: Bool
  } deriving (Eq, Ord, Show, Generic)

defaultOpenDatabaseOptions :: OpenDatabaseOptions
defaultOpenDatabaseOptions = OpenDatabaseOptions Nothing Nothing True

getProgram :: GhidraState -> Ghidra J.ProgramDB
getProgram = return . view #program

getTaskMonitor :: GhidraState -> Ghidra J.TaskMonitor
getTaskMonitor = return . view #taskMonitor

getFlatDecompilerAPI :: GhidraState -> Ghidra J.FlatDecompilerAPI
getFlatDecompilerAPI = return . view #flatDecompilerAPI

getLang :: Text -> Ghidra (Maybe J.Language)
getLang lang = do
  langProvider :: J.SleighLanguageProvider <- runIO $ Java.new >>= JNI.newGlobalRef
  maybeNullCall (runIO $ Java.reflect lang >>= Java.new >>= JNI.newGlobalRef) >>= \case
    Nothing -> return Nothing
    Just (langId :: J.LanguageID) -> do
      maybeNullCall . runIO $ Java.call langProvider "getLanguage" langId >>= JNI.newGlobalRef

getCSpec :: J.Language -> Maybe Text -> Ghidra J.CompilerSpec
getCSpec lang Nothing = runIO $ Java.call lang "getDefaultCompilerSpec"
getCSpec lang (Just compilerName) = runIO $ do
  compSpecId :: J.CompilerSpecID <- Java.reflect compilerName >>= Java.new >>= JNI.newGlobalRef
  Java.call lang "getCompilerSpecByID" compSpecId >>= JNI.newGlobalRef

hasBeenAnalyzed :: GhidraState -> Ghidra Bool
hasBeenAnalyzed gs = runIO $ do
  programInfoField :: J.String <- Java.getStaticField "ghidra.program.model.listing.Program" "PROGRAM_INFO" >>= JNI.newGlobalRef
  analyzedField :: J.String <- Java.getStaticField "ghidra.program.model.listing.Program" "ANALYZED_OPTION_NAME" >>= Java.reify >>= JNI.newGlobalRef
  prgOptions :: J.Options <- Java.call (gs ^. #program) "getOptions" programInfoField >>= JNI.newGlobalRef
  Java.call prgOptions "getBoolean" analyzedField False


data OpenDatabaseError
  = CouldNotFindFile Text
  | CouldNotFindLang Text
  | ImportByUsingBestGuessError Text
  | ImportByLookingForLcsError Text
  deriving (Eq, Ord, Show, Generic)

-- | Opens binary as Ghidra database.
openDatabase'
  :: OpenDatabaseOptions
  -> FilePath
  -> Ghidra (Either OpenDatabaseError GhidraState)
openDatabase' opts fp = do
  config :: J.HeadlessGhidraApplicationConfiguration <- runIO $ Java.new >>= JNI.newGlobalRef
  layout :: J.GhidraJarApplicationLayout <- runIO $ Java.new >>= JNI.newGlobalRef
  _ :: () <- bool identity suppressOut (opts ^. #quiet) . runIO $ do
    Java.callStatic "ghidra.framework.Application" "isInitialized" >>= \case
      True -> return ()
      False -> Java.callStatic "ghidra.framework.Application" "initializeApplication" (coerce layout :: J.ApplicationLayout) (coerce config :: J.ApplicationConfiguration)
  consumer :: J.Object <- runIO $ Java.new >>= JNI.newGlobalRef
  messageLog :: J.MessageLog <- runIO $ Java.new >>= JNI.newGlobalRef
  tm :: J.TaskMonitor <- runIO $ Java.getStaticField "ghidra.util.task.TaskMonitor" "DUMMY" >>= JNI.newGlobalRef
  file :: J.File <- runIO $ Java.reflect (cs fp :: Text) >>= Java.new >>= JNI.newGlobalRef
  runExceptT $ do
    results :: J.LoadResults J.Program <- case opts ^. #language of
      Nothing ->
        liftEitherM . fmap (first ImportByUsingBestGuessError) . tryJVM . runIO
        $ Java.callStatic
          "ghidra.app.util.importer.AutoImporter"
          "importByUsingBestGuess"
          file
          (Java.jnull :: J.Project)
          (Java.jnull :: J.String)
          consumer
          messageLog
          tm
          >>= JNI.newGlobalRef
      Just lang -> do
        lang' <- liftMaybeM (CouldNotFindLang lang) $ getLang lang
        cspec <- lift . getCSpec lang' $ opts ^. #compiler
        liftEitherM . fmap (first ImportByLookingForLcsError) . tryJVM . runIO
          $ Java.callStatic
            "ghidra.app.util.importer.AutoImporter"
            "importByLookingForLcs"
            file
            (Java.jnull :: J.Project)
            (Java.jnull :: J.String)
            lang'
            cspec
            consumer
            messageLog
            tm
            >>= JNI.newGlobalRef
    lift $ do
      resultsIter :: J.Iterator (J.Loaded J.Program) <- runIO $ Java.call results "iterator" >>= JNI.newGlobalRef
      loadedProgs :: [J.Loaded J.Program] <- iteratorToList resultsIter
      -- Presently, this object implementing the `Program` interface is always
      -- a `ProgramDB` class instance.
      prgs :: [J.Program] <- traverse getDomainObject loadedProgs
      let prg = head prgs
      flatApi :: J.FlatProgramAPI <- runIO $ Java.new prg >>= JNI.newGlobalRef
      flatDecApi :: J.FlatDecompilerAPI <- runIO $ Java.new flatApi >>= JNI.newGlobalRef
      return $ GhidraState tm (coerce prg :: J.ProgramDB) flatApi flatDecApi

openDatabase :: FilePath -> Ghidra (Either OpenDatabaseError GhidraState)
openDatabase = openDatabase' defaultOpenDatabaseOptions

openDatabase_ :: FilePath -> Ghidra GhidraState
openDatabase_ = fmap unsafeFromRight . openDatabase


data AnalyzeOptions = AnalyzeOptions
  { force :: Bool
  , quiet :: Bool
  } deriving (Eq, Ord, Show, Generic)

defaultAnalyzeOptions :: AnalyzeOptions
defaultAnalyzeOptions = AnalyzeOptions False True

analyze' :: AnalyzeOptions -> GhidraState -> Ghidra ()
analyze' opts gs = do
  alreadyAnalyzed <- hasBeenAnalyzed gs
  when (not alreadyAnalyzed || (opts ^. #force)) $
    bool identity suppressOut (opts ^. #quiet) . runIO $ do
      let prg = gs ^. #program
      txId :: Int32 <- Java.call prg "startTransaction" =<< Java.reflect ("Analysis" :: Text)
      _ :: () <- Java.call (gs ^. #flatProgramAPI) "analyzeAll" (coerce prg :: J.Program)
      _ :: () <- Java.callStatic "ghidra.program.util.GhidraProgramUtilities" "markProgramAnalyzed" (coerce prg :: J.Program)
      _ :: () <- Java.call prg "endTransaction" txId True
      return ()

analyze :: GhidraState -> Ghidra ()
analyze = analyze' defaultAnalyzeOptions

getListing :: GhidraState -> Ghidra J.Listing
getListing gs = do
  prg <- getProgram gs
  runIO $ Java.call prg "getListing" >>= JNI.newGlobalRef

-- | Adds address to image base.
-- Only use this with PIE binaries.
mkAddressBased :: GhidraState -> BA.Address -> Ghidra J.Address
mkAddressBased gs addr = do
  prg <- getProgram gs
  baseAddr :: J.Address <- runIO $ Java.call prg "getImageBase" >>= JNI.newGlobalRef
  runIO $ Java.call baseAddr "add" (fromIntegral addr :: Int64)

-- | Makes a new address
mkAddress :: GhidraState -> BA.Address -> Ghidra J.Address
mkAddress gs addr = do
  prg <- getProgram gs
  baseAddr :: J.Address <- runIO $ Java.call prg "getImageBase" >>= JNI.newGlobalRef
  runIO $ Java.call baseAddr "getNewAddress" (fromIntegral addr :: Int64)

saveDatabase :: GhidraState -> FilePath -> Ghidra ()
saveDatabase gs fp = runIO $ do
  let prg = gs ^. #program
  () <- Java.call prg "updateMetadata"
  jstringFilePath <- Java.reflect . Text.pack $ fp
  file :: J.File <- Java.new jstringFilePath
  () <- Java.call (coerce prg :: J.DomainObjectAdapterDB) "saveToPackedFile" file (gs ^. #taskMonitor)
  return ()
