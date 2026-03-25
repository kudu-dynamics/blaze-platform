module Ghidra.State where

import Ghidra.Prelude hiding (force, get)

import qualified Language.Java as Java

import qualified Ghidra.Program as Program
import qualified Ghidra.Address as Addr
import Ghidra.Address (AddressSpaceMap)
import Ghidra.Util (iteratorToList, maybeNullCall, suppressOut, tryJVM, getDomainObject, maybeNull)
import qualified Ghidra.Types as J
import Ghidra.Types.Internal (Ghidra, runIO)

import qualified Data.BinaryAnalysis as BA
import qualified Data.HashMap.Strict as HashMap
import qualified Foreign.JNI as JNI
import qualified Data.Text as Text


data GhidraState = GhidraState
  { taskMonitor :: J.TaskMonitor
  , program :: J.ProgramDB
  , flatProgramAPI :: J.FlatProgramAPI
  , flatDecompilerAPI :: J.FlatDecompilerAPI
  , addressSpaceMap :: AddressSpaceMap
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
    Nothing -> do
      return Nothing
    Just (langId :: J.LanguageID) -> do
      r <- maybeNullCall . runIO $ Java.call langProvider "getLanguage" langId >>= JNI.newGlobalRef
      return $ r >>= maybeNull

getCSpec :: J.Language -> Maybe Text -> Ghidra J.CompilerSpec
getCSpec lang Nothing = runIO $ Java.call lang "getDefaultCompilerSpec"
getCSpec lang (Just compilerName) = runIO $ do
  compSpecId :: J.CompilerSpecID <- Java.reflect compilerName >>= Java.new >>= JNI.newGlobalRef
  Java.call lang "getCompilerSpecByID" compSpecId >>= JNI.newGlobalRef

getSP :: J.ProgramDB -> Ghidra J.Register
getSP prg = do
  cmps :: J.CompilerSpec <- runIO $ Java.call prg "getCompilerSpec" -- getCSpec?
  reg :: J.Register <- runIO $ Java.call cmps "getStackPointer"
  return reg

hasBeenAnalyzed :: J.ProgramDB -> Ghidra Bool
hasBeenAnalyzed prg = runIO $ do
  programInfoField :: J.String <- Java.getStaticField "ghidra.program.model.listing.Program" "PROGRAM_INFO" >>= JNI.newGlobalRef
  analyzedField :: J.String <- Java.getStaticField "ghidra.program.model.listing.Program" "ANALYZED_OPTION_NAME" >>= Java.reify >>= JNI.newGlobalRef
  prgOptions :: J.Options <- Java.call prg "getOptions" programInfoField >>= JNI.newGlobalRef
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
      prg <- maybe (error "Couldn't find any J.Program for Ghidra db") return $ headMay prgs
      flatApi :: J.FlatProgramAPI <- runIO $ Java.new prg >>= JNI.newGlobalRef
      flatDecApi :: J.FlatDecompilerAPI <- runIO $ Java.new flatApi >>= JNI.newGlobalRef
      addrSpaces <- Program.getAddressSpaceMap (coerce prg :: J.ProgramDB)
      return $ GhidraState tm (coerce prg :: J.ProgramDB) flatApi flatDecApi addrSpaces

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
  let prg = gs ^. #program
  alreadyAnalyzed <- hasBeenAnalyzed prg
  when (not alreadyAnalyzed || (opts ^. #force)) $
    bool identity suppressOut (opts ^. #quiet) . runIO $ do
      txId :: Int32 <- Java.call prg "startTransaction" =<< Java.reflect ("Analysis" :: Text)
      _ :: () <- Java.call (gs ^. #flatProgramAPI) "analyzeAll" (coerce prg :: J.Program)
      _ :: () <- Java.callStatic "ghidra.program.util.GhidraProgramUtilities" "markProgramAnalyzed" (coerce prg :: J.Program)
      _ :: Bool <- Java.call prg "endTransaction" txId True
      return ()

analyze :: GhidraState -> Ghidra ()
analyze = analyze' defaultAnalyzeOptions

getListing :: J.ProgramDB -> Ghidra J.Listing
getListing prg = do
  runIO $ Java.call prg "getListing" >>= JNI.newGlobalRef

-- | Adds address to image base.
-- Only use this with PIE binaries.
mkAddressBased :: J.ProgramDB -> Int64 -> Ghidra J.Address
mkAddressBased prg addr = do
  baseAddr <- getImageBase prg
  runIO $ Java.call baseAddr "add" addr

mkExternalAddress :: J.ProgramDB -> Int64 -> Ghidra J.Address
mkExternalAddress prg offset = do
  af <- Program.getAddressFactory prg
  externSpace <- fromJust <$> Addr.getAddressSpace af (Addr.showAddressSpaceName BA.EXTERNAL)
  externSpaceID <- Addr.getSpaceID externSpace
  Addr.getAddress af externSpaceID offset

getImageBase :: J.ProgramDB -> Ghidra J.Address
getImageBase prg = do
  runIO $ Java.call prg "getImageBase" >>= JNI.newGlobalRef

-- | Makes a new address
mkAddress_ :: J.ProgramDB -> Int64 -> Ghidra J.Address
mkAddress_ prg addr = do
  baseAddr <- getImageBase prg
  runIO $ Java.call baseAddr "getNewAddress" addr

-- | Makes a new address
mkAddress :: J.ProgramDB -> BA.Address -> Ghidra J.Address
mkAddress prg addr = do
  mkAddress_ prg (BA.addrToInt addr)

getMemoryMap :: J.ProgramDB -> Ghidra J.MemoryMapDB
getMemoryMap prg = do
  runIO $ Java.call prg "getMemory"  >>= JNI.newGlobalRef

getMemoryBlockAtAddress :: J.ProgramDB -> Int64 -> Ghidra (Maybe J.MemoryBlock)
getMemoryBlockAtAddress prg addr = do
  mm :: J.MemoryMapDB <- getMemoryMap prg
  anAddr <- mkAddress_ prg addr
  maybeNull <$> runIO ( Java.call mm "getBlock" anAddr  >>= JNI.newGlobalRef)
  
getSegmentBlockName :: J.MemoryBlock -> Ghidra Text
getSegmentBlockName memBlock = do
  runIO $ Java.call memBlock "getName" >>=  Java.reify

getSegmentBlockFromAddress :: J.ProgramDB -> Int64 -> Ghidra (Maybe Text)
getSegmentBlockFromAddress prg addr = do
  mblock <- getMemoryBlockAtAddress prg addr
  case mblock of
    Nothing -> return Nothing
    Just block -> do
      Just <$> getSegmentBlockName block

saveDatabase :: GhidraState -> FilePath -> Ghidra ()
saveDatabase gs fp = runIO $ do
  let prg = gs ^. #program
  () <- Java.call prg "updateMetadata"
  jstringFilePath <- Java.reflect . Text.pack $ fp
  file :: J.File <- Java.new jstringFilePath
  () <- Java.call (coerce prg :: J.DomainObjectAdapterDB) "saveToPackedFile" file (gs ^. #taskMonitor)
  return ()

-- | Iterate all defined data items in the binary and collect string entries.
-- Returns a map from address offset to string value.
getDefinedStrings :: J.ProgramDB -> Ghidra (HashMap Int64 Text)
getDefinedStrings prg = do
  listing <- getListing prg
  mm <- getMemoryMap prg
  dataIter :: J.DataIterator <- runIO $
    Java.call listing "getDefinedData" (coerce mm :: J.AddressSetView) True
      >>= JNI.newGlobalRef
  dataItems <- dataIteratorToList dataIter
  HashMap.fromList . catMaybes <$> traverse extractString dataItems
  where
    dataIteratorToList :: J.DataIterator -> Ghidra [J.Data]
    dataIteratorToList x = do
      hasNext :: Bool <- runIO $ Java.call x "hasNext"
      if hasNext
        then do
          d :: J.Data <- runIO $ Java.call x "next" >>= JNI.newGlobalRef
          (d:) <$> dataIteratorToList x
        else return []

    extractString :: J.Data -> Ghidra (Maybe (Int64, Text))
    extractString d = do
      hasStr :: Bool <- runIO $ Java.call d "hasStringValue"
      if hasStr
        then do
          mStr <- maybeNullCall $ do
            valObj :: J.Object <- runIO $ Java.call d "getValue"
            runIO $ Java.reify (coerce valObj :: J.String)
          case mStr of
            Nothing -> return Nothing
            Just strVal -> do
              addr :: J.Address <- runIO $ Java.call d "getAddress"
              offset :: Int64 <- runIO $ Java.call addr "getOffset"
              return $ Just (offset, strVal)
        else return Nothing
