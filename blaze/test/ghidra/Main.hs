module Main where

import Prelude (IO)

import Blaze.Import.Binary (shutdown)
import Blaze.Import.Source.Ghidra (GhidraImporter)

import qualified Spec

main :: IO ()
main = do
  Spec.main
  shutdown @GhidraImporter
