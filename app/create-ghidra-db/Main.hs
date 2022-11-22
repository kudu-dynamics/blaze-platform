module Main where

import Ghidra.Prelude
import Ghidra.Core (runGhidra)
import Ghidra.State


main :: IO ()
main = runGhidra $ getArgs >>= \case
  [] -> do
    putText "./create-ghidra-db [path/to/binary1] [path/to/binary2.bin] ..."
    putText "-- Creates path/to/binary1.gfz path/to/binary2.bin.gfz, etc"
  args -> mapM_ openBinaryAndSave args

openBinaryAndSave :: FilePath -> IO ()
openBinaryAndSave fp = do
  let opts = defaultOpenDatabaseOptions & #quiet .~ False
  openDatabase' opts fp >>= \case
    Left err -> error $ "Could not load binary: " <> show err
    Right gs -> do
      analyze gs
      let fp' = fp <> ".gzf"
      saveDatabase gs fp'
      putText $ "Saved Ghidra db archive to: " <> show fp'
  
