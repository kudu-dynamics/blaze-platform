{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main (main) where

import Blaze.Import.Binary (openBinary)
import Blaze.Import.Source.BinaryNinja (BNImporter)
import Blaze.Import.Source.Ghidra qualified as G
import System.Console.CmdArgs

import Flint.Prelude

{- Usage:
./flint --backend <ghidra,binja> --file <path/to/binary>
1. Parse cmdline args
2. Open file
3. Choose backend
4. Lift binary with backend of choice
5. Run queries against binary
-}

data Backend = Binja | Ghidra deriving (Data, Show, Eq)

data Options = Options
  { backend :: Backend
  , files :: String
  }
  deriving (Data, Typeable)

options :: Options
options =
  Options
    { backend = enum [Binja &= help "Use binja backend", Ghidra &= help "Use ghidra backend"]
    , files =
        ""
          &= typ "FILES/DIRS"
          &= help "Choose a binary to analyze"
    }
    &= verbosity
    &= help "Run search queries against binary executables"
    &= summary "Flint"
    &= program "flint"
    &= details ["Flint is a static analysis tool that can run queries on binary code"]

parseArgs :: Options -> (Backend, String)
parseArgs Options{backend = b, files = f} = (b, f)

main :: IO ()
main = do
  opts <- cmdArgs options
  let (backend, files) = parseArgs opts
  if files == ""
    then putText "--files= is required!"
    else case backend of
      Binja -> do
        (openBinary files :: IO (Either Text BNImporter)) >>= \case
          Left err -> print err
          Right _bndb -> putText "Got bndb."
      Ghidra -> do
        _importer <- G.getImporter files
        putText "Got ghidradb."
  print backend
  print files
