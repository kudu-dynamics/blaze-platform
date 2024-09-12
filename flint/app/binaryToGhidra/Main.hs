module Main (main) where

import Flint.Prelude

import qualified Blaze.Import.Binary as BinImp
import Blaze.Import.Binary (openBinary, shutdown)
import Blaze.Import.Source.Ghidra (GhidraImporter)

import Options.Applicative


data Options = Options
  { inputBinary :: FilePath
  , outputFile :: Maybe FilePath
  }
  deriving (Eq, Ord, Read, Show, Generic)

parseInputBinary :: Parser FilePath
parseInputBinary = argument str
  ( metavar "INPUT_BINARY"
    <> help "input binary"
  )

parseOutputFile :: Parser FilePath
parseOutputFile = option str
  ( long "output"
    <> short 'o'
    <> metavar "OUTPUT_FILE"
    <> help "output file path"
  )

optionsParser :: Parser Options
optionsParser = Options
  <$> parseInputBinary
  <*> optional parseOutputFile

main :: IO ()
main = do
  opts <- execParser optsParser
  (egz :: Either Text GhidraImporter) <- openBinary $ opts ^. #inputBinary
  case egz of
    Left err -> error $ cs err
    Right gz -> do
      let outputFilePath = case opts ^. #outputFile of
            Nothing -> (opts ^. #inputBinary) <> ".gzf"
            Just fp -> fp
      BinImp.saveToDb outputFilePath gz >>= \case
        Left err -> error $ cs err
        Right fp -> putText $ "Saved to: " <> show fp
      shutdown @GhidraImporter
  where
    optsParser = info (optionsParser <**> helper)
      ( fullDesc
        <> progDesc "Converts a binary to a Ghidra gzf project file."
        <> header "binaryToGhidra" )
