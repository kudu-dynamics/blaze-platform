module Main (main) where

import Flint.Prelude

import Flint.App (withBackend, Backend)
import qualified Flint.Cfg.Store as Store
import qualified Flint.Types.CachedCalc as CC

import qualified Blaze.Persist.Db as Db

import Options.Applicative
import System.Directory (removeFile)
import System.Posix.Signals (killProcess, raiseSignal)


data Options = Options
  { backend :: Maybe Backend
  , inputFile :: FilePath
  , outputFlintDbFilePath :: FilePath
  }
  deriving (Eq, Ord, Read, Show, Generic)

parseBackend :: Parser Backend
parseBackend = option auto
  ( long "backend"
    <> metavar "BACKEND"
    <> help "preferred backend (BinaryNinja or Ghidra)"
  )

parseOutputFlintDbFilePath :: Parser FilePath
parseOutputFlintDbFilePath = argument str
  ( metavar "OUTPUT_FLINT_DB_FILEPATH"
    <> help "output filepath for Flint db"
  )

parseInputFile :: Parser FilePath
parseInputFile = argument str
  ( metavar "INPUT_FILE"
    <> help "input file"
  )

optionsParser :: Parser Options
optionsParser = Options
  <$> optional parseBackend
  <*> parseInputFile
  <*> parseOutputFlintDbFilePath

main :: IO ()
main = do
  opts <- execParser optsParser
  withBackend (opts ^. #backend) (opts ^. #inputFile) $ \imp -> do
    store <- Store.init Nothing imp
    (Just cg) <- CC.get () $ store ^. #callGraphCache
    catch (removeFile $ opts ^. #outputFlintDbFilePath) $ \(_ :: SomeException) -> return ()
    conn <- Db.init $ opts ^. #outputFlintDbFilePath
    Db.insertCallGraph conn cg
    putText $ "Wrote Flint db to: " <> show (opts ^. #outputFlintDbFilePath)
    raiseSignal killProcess
  where
    optsParser = info (optionsParser <**> helper)
      ( fullDesc
        <> progDesc "Generates a Flint analysis db for a binary"
        <> header "gen_flint_db" )
