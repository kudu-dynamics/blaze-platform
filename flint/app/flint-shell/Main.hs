{-# LANGUAGE CPP #-}

module Main where

import Flint.Prelude

import Flint.App (withBackend, Backend)
import qualified Flint.Cfg.Store as Store
import Flint.Shell.Types (initShellState)
import Flint.Shell.Repl (runShell)

import Blaze.Import.Binary (getBase)

import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory (doesFileExist)
import Options.Applicative hiding (info)
import qualified Options.Applicative as OA


data VerbosityLevel = Info | Warn | Debug deriving (Eq, Ord, Read, Show, Generic)

data ShellOptions = ShellOptions
  { backend        :: Maybe Backend
  , doNotUseSolver :: Bool
  , analysisDb     :: Maybe FilePath
  , verbosity      :: VerbosityLevel
  , typeHintsFile  :: Maybe FilePath
  , inputFile      :: FilePath
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
  <> help "DB to save and load analysis data"

parseDoNotUseSolver :: Parser Bool
parseDoNotUseSolver = switch $
  long "doNotUseSolver"
  <> help "do not enable the SMT solver by default"

parseTypeHintsFile :: Parser FilePath
parseTypeHintsFile = strOption $
  long "typeHints"
  <> short 't'
  <> metavar "TYPEHINT_FILE"
  <> help "file containing functions that we should get type hints for"

parseInputFile :: Parser FilePath
parseInputFile = argument str $
  metavar "INPUT_FILE"
  <> help "input binary file (e.g. .gzf)"

optionsParser :: Parser ShellOptions
optionsParser = ShellOptions
  <$> optional parseBackend
  <*> (parseDoNotUseSolver <|> pure False)
  <*> optional parseAnalysisDb
  <*> (parseVerbosity <|> pure Info)
  <*> optional parseTypeHintsFile
  <*> parseInputFile

main :: IO ()
main = do
  opts <- execParser optsParser
  setVerbosity $ case opts ^. #verbosity of
    Info -> VInfo
    Warn -> VWarn
    Debug -> VDebug
  let fp = opts ^. #inputFile
  exists <- doesFileExist fp
  unless exists $ do
    putText $ "Error: file not found: " <> Text.pack fp
    exitFailure
  putText $ "Loading " <> Text.pack fp <> "..."
  withBackend (opts ^. #backend) fp $ \imp -> do
    typeHintsWhitelist <- maybe (pure HashSet.empty) getFuncsFromFile (opts ^. #typeHintsFile)
    (store, _funcToTypeHintsMap) <- Store.initWithTypeHints typeHintsWhitelist HashSet.empty (opts ^. #analysisDb) imp
    base <- getBase imp
    shellState <- initShellState store base (not $ opts ^. #doNotUseSolver)
    putText "Flint interactive shell. Type 'help' for available commands."
    runShell shellState
  where
    optsParser = OA.info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Interactive shell for exploring binaries with Flint."
     <> header "flint-shell" )

getFuncsFromFile :: FilePath -> IO (HashSet Text)
getFuncsFromFile fp
  = HashSet.fromList
  . filter (not . Text.null)
  . fmap Text.strip
  . Text.lines
  <$> liftIO (TextIO.readFile fp)
