module Ghidra.State where

import Ghidra.Prelude hiding (force)

import Language.Clojure
import Foreign.JNI.Types (JObject)
import qualified Data.Text as Text
import qualified Language.Java as Java
import System.IO.Memoize (once)
import qualified Language.Clojure.Map as ClojureMap
import Ghidra.Util (convertOpt)

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
