{-# LANGUAGE CPP #-}

module Main where

import Flint.Prelude

import Flint.SMTish (toFlintSMTishResult)
import Flint.Types.Analysis.Path.Matcher (Prim)
import qualified Flint.Types.Analysis.Path.Matcher as M
import Flint.Types.Analysis.Path.Matcher.Primitives (CallableWMI, PrimSpec)
import qualified Flint.Analysis.Path.Matcher.Primitives.Library as PrimLib
import Flint.Analysis.Path.Matcher.Primitives.Library.StdLib (allStdLibPrims)
import Flint.App (withBackend, Backend)
import qualified Flint.Cfg.Store as Store
import Flint.Query
import qualified Flint.Types.CachedMap as CM

import Blaze.Import.Binary (getBase)
import Blaze.Pretty (pretty')
import qualified Blaze.Types.Function as Func

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Aeson (ToJSON(toJSON))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Aeson.Encode.Pretty (encodePretty)
import Options.Applicative hiding (info)
import qualified Options.Applicative as OA

data VerbosityLevel = Info | Warn | Debug deriving (Eq, Ord, Read, Show, Generic)

-- Flint: the onion version

data Options = Options
  { backend :: Maybe Backend
  , outputToFile :: Maybe FilePath
  , doNotUseSolver :: Bool
  , onionDepth :: Word64
  , pathSamplingFactor :: Double
  , isKernelModule :: Bool
  , analysisDb :: Maybe FilePath
  , verbosity :: VerbosityLevel
  , filterFuncsFile :: Maybe FilePath
  , blacklistFile :: Maybe FilePath
  , typeHintsFile :: Maybe FilePath
  , outputSMTish :: Bool
  , inputFile :: FilePath
  }
  deriving (Eq, Ord, Read, Show, Generic)

parseBackend :: Parser Backend
parseBackend = option auto $
  long "backend"
  <> metavar "BACKEND"
  <> help
     ( "preferred backend ("
#ifdef FLINT_SUPPORT_BINARYNINJA
         <> "BinaryNinja or "
#endif
         <> "Ghidra)"
     )

parseOutputToFile :: Parser FilePath
parseOutputToFile = strOption $
  long "outputToFile"
  <> short 'o'
  <> metavar "OUTPUT_TO_FILE"
  <> help "Output results to file"

parseVerbosity :: Parser VerbosityLevel
parseVerbosity = option auto $
  long "verbosity"
  <> metavar "VERBOSITY"
  <> help "verbosity (Info | Warn | Debug)"

parseAnalysisDb :: Parser FilePath
parseAnalysisDb = strOption $
  long "analysisDb"
  <> metavar "ANALYSIS_DB"
  <> help "DB to save a load analysis data"

parseOnionDepth :: Parser Word64
parseOnionDepth = option auto $
  long "onionDepth"
  <> metavar "ONION_DEPTH"
  <> help "number of layers to bubble up primitives thru callsites"

parsePathSamplingFactor :: Parser Double
parsePathSamplingFactor = option auto $
  long "pathSamplingFactor"
  <> metavar "PATH_SAMPLING_FACTOR"
  <> help "adjust sampling multiplier (default 1.0)"

parseIsKernelModule :: Parser Bool
parseIsKernelModule = switch $
  long "isKernelModule"
  <> help "do lifecyle check for kernel modules"

parseInputFile :: Parser FilePath
parseInputFile = argument str $
  metavar "INPUT_FILE"
  <> help "input file"

parseDoNotUseSolver :: Parser Bool
parseDoNotUseSolver = switch $
  long "doNotUseSolver"
  <> help "do not verify if paths are satisfiable"

parseFilterFuncsFile :: Parser FilePath
parseFilterFuncsFile = strOption $
  long "filterFuncs"
  <> short 'f'
  <> metavar "FILTER_FUNCS_FILE"
  <> help "file containing function names to include in JSON output"

parseBlacklistFile :: Parser FilePath
parseBlacklistFile = strOption $
  long "blacklist"
  <> short 'b'
  <> metavar "BLACKLIST_FILE"
  <> help "file containing functions names to skip during analysis"

parseTypeHintsFile :: Parser FilePath
parseTypeHintsFile = strOption $
  long "typeHints"
  <> short 't'
  <> metavar "TYPEHINT_FILE"
  <> help "file containing functions that we should get type hints for"

parseOutputSMTish :: Parser Bool
parseOutputSMTish = switch $
  long "outputSMTish"
  <> help "output SMT-like syntax instead of PIL"

optionsParser :: Parser Options
optionsParser = Options
  <$> optional parseBackend
  <*> optional parseOutputToFile
  <*> (parseDoNotUseSolver <|> pure False)
  <*> (parseOnionDepth <|> pure 3)
  <*> (parsePathSamplingFactor <|> pure 1.0)
  <*> (parseIsKernelModule <|> pure False)
  <*> optional parseAnalysisDb
  <*> (parseVerbosity <|> pure Info)
  <*> optional parseFilterFuncsFile
  <*> optional parseBlacklistFile
  <*> optional parseTypeHintsFile
  <*> (parseOutputSMTish <|> pure False)
  <*> parseInputFile

main :: IO ()
main = do
  opts <- execParser optsParser
  setVerbosity $ case opts ^. #verbosity of
    Info -> VInfo
    Warn -> VWarn
    Debug -> VDebug
  onionCheck opts
  where
    optsParser = OA.info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Static path-based analysis to find bugs."
     <> header "Flint" )

printJSON :: MatchingResult -> IO ()
printJSON res = do
  let func = res ^. #func
      name = func ^. #name
      addr = func ^. #address
      blob = MatchingResultBlob
        { func = (name, addr)
        , pathAsStmts = pretty' <$> res ^. #pathAsStmts
        , bugName = res ^. #bugName
        , mitigationAdvice = res ^. #mitigationAdvice
        , bugDescription = res ^. #bugDescription
        }
  sequentialPutText . Text.pack . unpack . encodePretty . toJSON $ blob

toMatchingPrimBlob :: MatchingPrim -> MatchingPrimBlob
toMatchingPrimBlob res = blob
  where
    func = res ^. #func
    name = func ^. #name
    addr = func ^. #address
    wmi = toCallableWMIBlob $ res ^. #callablePrim
    blob = MatchingPrimBlob
      { func = (name, addr)
      , path = pretty' <$> res ^. #path -- TODO: get path with assertions
      , primName = res ^. #callablePrim . #prim . #name
      , vars = HashMap.fromList
               . fmap (\(k, v) -> (pretty' k, pretty' $ fst v))
               . HashMap.toList
               $ res ^. #callablePrim . #varMapping
      , locations = wmi ^. #locations
      , constraints = fmap (pretty' . fst) $ res ^. #callablePrim . #constraints
      , linkedVars = fmap pretty' . HashSet.toList $ res ^. #callablePrim . #linkedVars
      }


printMatchingPrimJSON :: MatchingPrim -> IO ()
printMatchingPrimJSON res = do
  let func = res ^. #func
      name = func ^. #name
      addr = func ^. #address
      wmi = toCallableWMIBlob $ res ^. #callablePrim
      blob = MatchingPrimBlob
        { func = (name, addr)
        , path = pretty' <$> res ^. #path -- TODO: get path with assertions
        , primName = res ^. #callablePrim . #prim . #name
        , vars = HashMap.fromList
                 . fmap (\(k, v) -> (pretty' k, pretty' $ fst v))
                 . HashMap.toList
                 $ res ^. #callablePrim . #varMapping
        , locations = wmi ^. #locations
        , constraints = pretty' <$> res ^. #callablePrim . #constraints
        , linkedVars = fmap pretty' . HashSet.toList $ res ^. #callablePrim . #linkedVars
        }
  sequentialPutText . Text.pack . unpack . encodePretty . toJSON $ blob

printCallablePrimsJSON :: (PrimSpec, HashSet CallableWMI) -> IO ()
printCallablePrimsJSON (primtype, cprims) = do
  let blob = ( primtype ^. #name
             , fmap toCallableWMIBlob
               . HashSet.toList
               $ cprims
             )
  sequentialPutText . Text.pack . unpack . encodePretty . toJSON $ blob

resultToJSON :: ToJSON a => a -> Text
resultToJSON = Text.pack . unpack . encodePretty . toJSON

printResult :: ToJSON a => a -> IO ()
printResult = sequentialPutText . resultToJSON

writeResult :: ToJSON a => FilePath -> a -> IO ()
writeResult fp = TextIO.writeFile fp . resultToJSON

toFlintResult
  :: Address
  -> HashMap PrimSpec (HashSet CallableWMI)
  -> FlintResult
toFlintResult baseOffset
  = FlintResult baseOffset
  . fmap (\(pt, s) -> (pt ^. #name, fmap toCallableWMIBlob . HashSet.toList $ s))
  . HashMap.toList


-- | Checks for bugs using the onion
onionCheck :: MonadIO m => Options -> m ()
onionCheck opts = withBackend (opts ^. #backend) (opts ^. #inputFile) $ \imp -> do
  typeHintsWhitelist <- maybe (pure HashSet.empty) getFuncsFromFile (opts ^. #typeHintsFile)
  (store,funcToTypeHintsMap) <- Store.initWithTypeHints typeHintsWhitelist (opts ^. #analysisDb) imp
  base <- getBase imp
  blacklist <- maybe (pure HashSet.empty) getFuncsFromFile (opts ^. #blacklistFile)
  let stdLibPrims = allStdLibPrims
      prims :: [Prim]
      prims = PrimLib.allPrims

  -- TODO: make maxResultsPerPath an option
  let maxResultsPerPath = 10 -- max WMIs found per path
  onionFlow
    maxResultsPerPath
    (not $ opts ^. #doNotUseSolver) 
    (opts ^. #onionDepth)
    (opts ^. #pathSamplingFactor)
    store 
    stdLibPrims 
    prims 
    blacklist 
    funcToTypeHintsMap
  cprims <- CM.getSnapshot $ store ^. #callablePrims
  filterFuncs <- maybe (pure Nothing) (fmap Just . getFuncsFromFile) (opts ^. #filterFuncsFile)
  let filteredCprims = case filterFuncs of
        Nothing -> cprims
        Just funcs -> HashMap.filterWithKey (\(_,func) _ -> HashSet.member (func ^. Func._name) funcs) cprims
  let cprims' = M.asOldCallableWMIsMap filteredCprims  
  case opts ^. #outputSMTish of
    False -> handleResult $ toFlintResult base cprims'
    True -> handleResult $ toFlintSMTishResult base cprims'
  where
    handleResult :: ToJSON a => a -> IO ()
    handleResult flintResult = case opts ^. #outputToFile of
      Nothing -> printResult flintResult
      Just outputFilePath -> do
        writeResult outputFilePath flintResult
        putText $ "Wrote results to " <> show outputFilePath


getFuncsFromFile :: FilePath -> IO (HashSet Text)
getFuncsFromFile fp 
  = HashSet.fromList
  . filter (not . Text.null)
  . fmap Text.strip
  . Text.lines
  <$> liftIO (TextIO.readFile fp)







