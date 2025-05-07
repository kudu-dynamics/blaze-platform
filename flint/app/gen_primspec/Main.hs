module Main (main) where

import Flint.Prelude

import qualified Data.Yaml as Yaml
import Options.Applicative


data Options = Options
  { inputSpecFile :: Maybe FilePath
  , outputModule :: Maybe FilePath
  }
  deriving (Eq, Ord, Read, Show, Generic)

parseInputSpecFile :: Parser FilePath
parseInputSpecFile = option str
  ( long "inputSpecFile"
    <> short 'i'
    <> metavar "INPUT_SPEC_FILE"
    <> help "file containing the primitive specs (default res/primspec.yaml)"
  )

parseOutputModule :: Parser FilePath
parseOutputModule = option str
  ( long "outputModule"
    <> short 'o'
    <> metavar "OUTPUT_MODULE"
    <> help "output Haskell module path"
  )

optionsParser :: Parser Options
optionsParser = Options
  <$> optional parseInputSpecFile
  <*> optional parseOutputModule

main :: IO ()
main = do
  opts <- execParser optsParser
  return ()
  where
    optsParser = info (optionsParser <**> helper)
      ( fullDesc
        <> progDesc "Creates the PrimSpec.hs Haskell module from a primspec.yaml file"
        <> header "gen_primspecn" )
