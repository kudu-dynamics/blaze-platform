module Main where

import Blaze.Prelude

import Blaze.Import.Source.BinaryNinja.MLIL (getInstructionsWithOpByName)
import qualified Data.Text as Text
import Blaze.Import.Binary (BinaryImporter(shutdown))
import Blaze.Import.Source.BinaryNinja (BNImporter)

main :: IO ()
main = getArgs >>= \case
  [opName, binPath] -> do
    xs <- getInstructionsWithOpByName (Text.pack opName) binPath
    mapM_ p xs
    putText "[finished]"
    shutdown @BNImporter
    exitSuccess
    where
      p (fn, ix) = putText $ fn <> " " <> show ix
  _ -> putText "find_mlil [op name] [binary path]"

