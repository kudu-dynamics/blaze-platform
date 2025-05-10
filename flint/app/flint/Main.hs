{-# LANGUAGE CPP #-}

module Main where

import Flint.Prelude

import Flint.Types.Analysis.Path.Matcher (Prim)
import Flint.Types.Analysis.Path.Matcher.Primitives (CallableWMI, PrimSpec)
import qualified Flint.Analysis.Path.Matcher.Primitives.Library as PrimLib
import Flint.Analysis.Path.Matcher.Primitives.Library.StdLib (allStdLibPrims)
import Flint.App (withBackend, Backend)
import qualified Flint.Cfg.Store as Store
import Flint.Query
import Flint.Util (sequentialPutText)
import qualified Flint.Types.CachedMap as CM

import Blaze.Import.Binary (getBase)
import Blaze.Pretty (pretty')

import Control.Monad.Logger (LogLevel(LevelInfo))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Data.Aeson (ToJSON(toJSON))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Aeson.Encode.Pretty (encodePretty)
import Options.Applicative

-- Flint: the onion version

data Options = Options
  { backend :: Maybe Backend
  , outputJSON :: Bool
  , doNotUseSolver :: Bool
  , onionDepth :: Word64
  , isKernelModule :: Bool
  , analysisDb :: Maybe FilePath
  , logLevel :: LogLevel
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

parseLogLevel :: Parser LogLevel
parseLogLevel = option auto $
  long "logLevel"
  <> metavar "LOGLEVEL"
  <> help "log level (LevelDebug | LevelInfo | LevelWarn | LevelError)"

parseAnalysisDb :: Parser FilePath
parseAnalysisDb = strOption $
  long "analysisDb"
  <> metavar "ANALYSIS_DB"
  <> help "DB to save a load analysis data"

parseJSONOption :: Parser Bool 
parseJSONOption = switch $
  long "outputJSON"
  <> help "output in a JSON format"

parseOnionDepth :: Parser Word64
parseOnionDepth = option auto $
  long "onionDepth"
  <> metavar "ONION_DEPTH"
  <> help "number of layers to bubble up primitives thru callsites"

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

optionsParser :: Parser Options
optionsParser = Options
  <$> optional parseBackend
  <*> (parseJSONOption <|> pure False)
  <*> (parseDoNotUseSolver <|> pure False)
  <*> (parseOnionDepth <|> pure 3)
  <*> (parseIsKernelModule <|> pure False)
  <*> optional parseAnalysisDb
  <*> (parseLogLevel <|> pure LevelInfo)
  <*> parseInputFile

main :: IO ()
main = do
  opts <- execParser optsParser
  runLoggerT (opts ^. #logLevel) $ onionCheck opts
  where
    optsParser = info (optionsParser <**> helper)
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
    blob = MatchingPrimBlob
      { func = (name, addr)
      , path = pretty' <$> res ^. #path -- TODO: get path with assertions
      , primName = res ^. #callablePrim . #prim . #name
      , vars = HashMap.fromList
               . fmap (\(k, v) -> (pretty' k, pretty' $ fst v))
               . HashMap.toList
               $ res ^. #callablePrim . #varMapping
      , locations = HashMap.mapKeys pretty' $ res ^. #callablePrim . #locations
      , constraints = fmap (pretty' . fst) $ res ^. #callablePrim . #constraints
      , linkedVars = fmap pretty' . HashSet.toList $ res ^. #callablePrim . #linkedVars
      }


printMatchingPrimJSON :: MatchingPrim -> IO ()
printMatchingPrimJSON res = do
  let func = res ^. #func
      name = func ^. #name
      addr = func ^. #address
      blob = MatchingPrimBlob
        { func = (name, addr)
        , path = pretty' <$> res ^. #path -- TODO: get path with assertions
        , primName = res ^. #callablePrim . #prim . #name
        , vars = HashMap.fromList
                 . fmap (\(k, v) -> (pretty' k, pretty' $ fst v))
                 . HashMap.toList
                 $ res ^. #callablePrim . #varMapping
        , locations = HashMap.mapKeys pretty' $ res ^. #callablePrim . #locations
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

printResult :: FlintResult -> IO ()
printResult = sequentialPutText . Text.pack . unpack . encodePretty . toJSON

toFlintResult
  :: Address
  -> HashMap PrimSpec (HashSet CallableWMI)
  -> FlintResult
toFlintResult baseOffset
  = FlintResult baseOffset
  . fmap (\(pt, s) -> (pt ^. #name, fmap toCallableWMIBlob . HashSet.toList $ s))
  . HashMap.toList

-- | Checks for bugs using the onion
onionCheck :: (MonadIO m, MonadLogger m) => Options -> m ()
onionCheck opts = withBackend (opts ^. #backend) (opts ^. #inputFile) $ \imp -> do
  store <- Store.init (opts ^. #analysisDb) imp
  base <- getBase imp
  let stdLibPrims = allStdLibPrims
      prims :: [Prim]
      prims = PrimLib.allPrims
        -- [ PrimLib.controlledFormatStringPrim
        -- ]

  onionFlow (not $ opts ^. #doNotUseSolver) (opts ^. #onionDepth) store stdLibPrims prims
  cprims <- CM.getSnapshot $ store ^. #callablePrims
  printResult . toFlintResult base $ cprims
  -- forM_ cprims printCallablePrimsJSON
  -- putText "dun"


