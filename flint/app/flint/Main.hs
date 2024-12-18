{-# LANGUAGE CPP #-}

module Main (main) where

import Flint.Prelude

import qualified Flint.Analysis.Path.Matcher.Patterns as Pat
import Flint.App (withBackend, Backend)
import qualified Flint.Cfg.Store as Store
import Flint.Query
import Flint.Util (sequentialPutText)

import Blaze.Function (Function)
import Blaze.Pretty (pretty')

import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Data.Aeson (ToJSON(toJSON))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Aeson.Encode.Pretty (encodePretty)
import Options.Applicative


data Options = Options
  { backend :: Maybe Backend
  , outputJSON :: Bool
  , doNotUseSolver :: Bool
  , maxSamplesPerFunc :: Word64
  , expandCallDepth :: Word64
  , analysisDb :: Maybe FilePath
  , inputFile :: FilePath
  }
  deriving (Eq, Ord, Read, Show, Generic)

parseBackend :: Parser Backend
parseBackend = option auto
  ( long "backend"
    <> metavar "BACKEND"
    <> help
       ( "preferred backend ("
#ifdef FLINT_SUPPORT_BINARYNINJA
         <> "BinaryNinja or "
#endif
         <> "Ghidra)"
       )
  )

parseAnalysisDb :: Parser FilePath
parseAnalysisDb = strOption
  ( long "analysisDb"
    <> metavar "ANALYSIS_DB"
    <> help "DB to save a load analysis data"
  )

parseJSONOption :: Parser Bool
parseJSONOption = switch
  ( long "outputJSON"
    <> help "output in a JSON format" )

parseMaxSamplesPerFunc :: Parser Word64
parseMaxSamplesPerFunc = option auto
  ( long "maxSamplesPerFunc"
    <> metavar "MAX_SAMPLES_PER_FUNC"
    <> help "max number of path samples to take per function"
  )

parseExpandCallDepth :: Parser Word64
parseExpandCallDepth = option auto
  ( long "expandCallDepth"
    <> metavar "EXPAND_CALL_DEPTH"
    <> help "depth of calls to expand"
  )

parseInputFile :: Parser FilePath
parseInputFile = argument str
  ( metavar "INPUT_FILE"
    <> help "input file"
  )

parseDoNotUseSolver :: Parser Bool
parseDoNotUseSolver = switch
  ( long "doNotUseSolver"
    <> help "do not verify if paths are satisfiable" )

optionsParser :: Parser Options
optionsParser = Options
  <$> optional parseBackend
  <*> (parseJSONOption <|> pure False)
  <*> (parseDoNotUseSolver <|> pure False)
  <*> (parseMaxSamplesPerFunc <|> pure 15)
  <*> (parseExpandCallDepth <|> pure 0)
  <*> optional parseAnalysisDb
  <*> parseInputFile

main :: IO ()
main = do
  opts <- execParser optsParser
  defaultCheck opts
  where
    optsParser = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Static path-based analysis to find bugs."
     <> header "Flint" )

printJSON :: MatchingResult -> IO()
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

-- | Checks for bugs by blindly sampling paths from every function
defaultCheck :: Options -> IO ()
defaultCheck opts = withBackend (opts ^. #backend) (opts ^. #inputFile) $ \imp -> do
  store <- Store.init (opts ^. #analysisDb) imp

  let q :: Query Function
      q = QueryExpandAll $ QueryExpandAllOpts
          { callExpandDepthLimit = opts ^. #expandCallDepth
          -- TODO: At some point, we should base the # samples on the size of func
          , numSamples = opts ^. #maxSamplesPerFunc
          }
      bms :: [BugMatch]
      bms =
        [ Pat.incrementWithoutCheck
        ]
      funcs :: HashSet Function
      funcs = HashSet.fromList $ store ^. #funcs
  let output = if opts ^. #outputJSON then printJSON else sequentialPutText . pretty'
  checkFuncs (not $ opts ^. #doNotUseSolver) store q bms output funcs
