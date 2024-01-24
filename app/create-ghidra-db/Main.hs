module Main where

import Ghidra.Prelude
import Ghidra.Core (runGhidraOrError)
import Ghidra.State


main :: IO ()
main = getArgs >>= \case
  [] -> do
    putText "./create-ghidra-db [path/to/binary1] [path/to/binary2.bin] ..."
    putText "-- Creates path/to/binary1.gzf path/to/binary2.bin.gzf, etc"
  args -> mapM_ openBinaryAndSave args

openBinaryAndSave :: FilePath -> IO ()
openBinaryAndSave fp = do
  let opts = defaultOpenDatabaseOptions & #quiet .~ False
  runGhidraOrError (openDatabase' opts fp) >>= \case
    Left err -> error $ "Could not load binary: " <> show err
    Right gs -> do
      runGhidraOrError $ analyze gs
      let fp' = fp <> ".gzf"
      runGhidraOrError $ saveDatabase gs fp'
      putText $ "Saved Ghidra db archive to: " <> show fp'
  
