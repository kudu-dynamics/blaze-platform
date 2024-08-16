module Main (main) where

import Flint.Prelude

import qualified Flint.Analysis.Path.Matcher.Patterns as Pat
import qualified Flint.Cfg.Store as Store
import Flint.Query
import Flint.Util (sequentialPutText)

import Blaze.Function (Function)
import Blaze.Pretty (pretty')
import Blaze.Import.Binary (openBinary)
import Blaze.Import.Source.BinaryNinja (BNImporter)
import Blaze.Import.Source.Ghidra qualified as G

import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Data.Aeson (ToJSON(toJSON))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Aeson.Encode.Pretty (encodePretty)
import Options.Applicative


data Backend
  = BinaryNinja
  | Ghidra
  deriving (Eq, Ord, Read, Show)

defaultBackend :: Backend
defaultBackend = BinaryNinja

data Options = Options
  { backend :: Maybe Backend
  , outputJSON :: Bool
  , doNotUseSolver :: Bool
  , maxSamplesPerFunc :: Word64
  , expandCallDepth :: Word64
  , inputFile :: FilePath
  }
  deriving (Eq, Ord, Read, Show, Generic)

parseBackend :: Parser Backend
parseBackend = option auto
  ( long "backend"
    <> metavar "BACKEND"
    <> help "preferred backend (BinaryNinja or Ghidra)"
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

guessFileBackend :: FilePath -> Maybe Backend
guessFileBackend fp
  | Text.isSuffixOf ".bndb" fp' = Just BinaryNinja
  | Text.isSuffixOf ".gzf" fp' = Just Ghidra
  | otherwise = Nothing
  where
    fp' = Text.pack fp

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
defaultCheck opts = do
  let (msg :: Text, backend') = case (opts ^. #backend, guessFileBackend $ opts ^. #inputFile) of
        (Nothing, Nothing) ->
          ( "Opening binary with default backend (" <> show defaultBackend <> ")"
          , BinaryNinja
          )
        (Nothing, Just b) ->
          ( "Opening " <> show b <> " db with " <> show b <> " backend"
          , b
          )
        (Just specifiedBackend, Nothing) ->
          ( "Opening binary with " <> show specifiedBackend <> " backend"
          , specifiedBackend
          )
        (Just specifiedBackend, Just guessedBackend)
          | specifiedBackend /= guessedBackend ->
            ( "WARNING: detected db file for " <> show guessedBackend <> " but using user-specified backend: " <> show specifiedBackend
            , specifiedBackend
            )
          | otherwise ->
            ( "Opening " <> show specifiedBackend <> " db with " <> show specifiedBackend <> " backend"
            , specifiedBackend
            )
  putText msg
  store <- case backend' of
    BinaryNinja -> do
      (ebv :: Either Text BNImporter) <- openBinary (opts ^. #inputFile)
      either (error . cs) Store.init ebv
    Ghidra -> do
      (egz :: Either Text G.GhidraImporter) <- openBinary (opts ^. #inputFile)
      either (error . cs) Store.init egz

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
