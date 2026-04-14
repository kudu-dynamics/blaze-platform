{-# LANGUAGE CPP #-}

module Main where

import Flint.Prelude

import Flint.Types.Analysis.Path.Matcher (Prim)
import qualified Flint.Analysis.Path.Matcher.Patterns as Pat
import Flint.App (withBackend, Backend)
import qualified Flint.Cfg.Store as Store
import Flint.Query

import Blaze.Function (Function)
import Blaze.Pretty (pretty')

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Data.Aeson (ToJSON(toJSON))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Aeson.Encode.Pretty (encodePretty)
import Options.Applicative hiding (info)
import qualified Options.Applicative as OA
import System.IO (hSetBuffering, BufferMode(..))


data VerbosityLevel = Info | Warn | Debug deriving (Eq, Ord, Read, Show, Generic)

data Options = Options
  { backend :: Maybe Backend
  , outputJSON :: Bool
  , doNotUseSolver :: Bool
  , maxSamplesPerFunc :: Word64
  , expandCallDepth :: Word64
  , isKernelModule :: Bool
  , analysisDb :: Maybe FilePath
  , verbosity :: VerbosityLevel
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

parseJSONOption :: Parser Bool 
parseJSONOption = switch $
  long "outputJSON"
  <> help "output in a JSON format"

parseIsKernelModule :: Parser Bool
parseIsKernelModule = switch $
  long "isKernelModule"
  <> help "do lifecyle check for kernel modules"

parseMaxSamplesPerFunc :: Parser Word64
parseMaxSamplesPerFunc = option auto $
  long "maxSamplesPerFunc"
  <> metavar "MAX_SAMPLES_PER_FUNC"
  <> help "max number of path samples to take per function"

parseExpandCallDepth :: Parser Word64
parseExpandCallDepth = option auto $
  long "expandCallDepth"
  <> metavar "EXPAND_CALL_DEPTH"
  <> help "depth of calls to expand"

parseInputFile :: Parser FilePath
parseInputFile = argument str $
  metavar "INPUT_FILE"
  <> help "input file"

parseDoNotUseSolver :: Parser Bool
parseDoNotUseSolver = switch $
  long "doNotUseSolver"
  <> help "do not verify if paths are satisfiable"

optionsParser :: Parser Options
optionsParser = Options
  <$> optional parseBackend
  <*> (parseJSONOption <|> pure False)
  <*> (parseDoNotUseSolver <|> pure False)
  <*> (parseMaxSamplesPerFunc <|> pure 15)
  <*> (parseExpandCallDepth <|> pure 0)
  <*> (parseIsKernelModule <|> pure False)
  <*> optional parseAnalysisDb
  <*> (parseVerbosity <|> pure Info)
  <*> parseInputFile

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  opts <- execParser optsParser
  setVerbosity $ case opts ^. #verbosity of
    Info -> VInfo
    Warn -> VWarn
    Debug -> VDebug
  primCheck opts
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
        , cwe = res ^. #cwe
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

-- | Checks for bugs by blindly sampling paths from every function
defaultCheck :: MonadIO m => Options -> m ()
defaultCheck opts = withBackend (opts ^. #backend) (opts ^. #inputFile) $ \imp -> do
  analysisDbPath <- Store.resolveAnalysisDb (opts ^. #analysisDb) (opts ^. #inputFile)
  store <- Store.init analysisDbPath imp
  funcs <- Store.getInternalFullFuncs store
  
  let q :: Query Function
      q = QueryExpandAll $ QueryExpandAllOpts
          { callExpandDepthLimit = opts ^. #expandCallDepth
          -- TODO: At some point, we should base the # samples on the size of func
          , numSamples = opts ^. #maxSamplesPerFunc
          , unrollLoops = False
          }
      bms :: [BugMatch]
      bms = Pat.allPatterns
      output = if opts ^. #outputJSON then printJSON else sequentialPutText . pretty'

  when (opts ^. #isKernelModule)
    $ checkKernelLifecycle
      (not $ opts ^. #doNotUseSolver)
      store
      (opts ^. #maxSamplesPerFunc)
      (opts ^. #expandCallDepth)
      output
      
  checkFuncs (not $ opts ^. #doNotUseSolver) store q bms output . HashSet.fromList $ funcs

-- | Checks for bugs by blindly sampling paths from every function
primCheck :: MonadIO m => Options -> m ()
primCheck opts = withBackend (opts ^. #backend) (opts ^. #inputFile) $ \imp -> do
  analysisDbPath <- Store.resolveAnalysisDb (opts ^. #analysisDb) (opts ^. #inputFile)
  store <- Store.init analysisDbPath imp
  funcs <- Store.getInternalFullFuncs store

  let q :: Query Function
      q = QueryExpandAll $ QueryExpandAllOpts
          { callExpandDepthLimit = opts ^. #expandCallDepth
          -- TODO: At some point, we should base the # samples on the size of func
          , numSamples = opts ^. #maxSamplesPerFunc
          , unrollLoops = False
          }
      prims :: [Prim]
      prims = []

  kernelResults <- case opts ^. #isKernelModule of
    False -> return []
    True -> checkKernelLifecycleForPrims'
      (not $ opts ^. #doNotUseSolver)
      store
      (opts ^. #maxSamplesPerFunc)
      (opts ^. #expandCallDepth)
      
  r <- checkFuncsForPrims' (not $ opts ^. #doNotUseSolver) store q prims . HashSet.fromList $ funcs
  putText . Text.pack . unpack . encodePretty . toJSON . fmap toMatchingPrimBlob $ r <> kernelResults
