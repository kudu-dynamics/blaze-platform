module Main where

import Binja.Header.ParseHeader (writeBeautyHeader)
import Binja.Header.Prelude

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inHeader, outHeader] -> do
      writeBeautyHeader inHeader outHeader
      putText "Success!"
    _ -> putText "clean-binja-header path/to/original/binaryninjacore.h path/to/new/binaryninjacore.h"
