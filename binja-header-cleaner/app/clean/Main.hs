module Main where

import Binja.Header.Prelude

import Binja.Header.ParseHeader (writeBeautyHeader)
--import qualified Data.Text.Lazy.IO as TextIO


main :: IO ()
main = do
  args <- getArgs
  case args of
    [inHeader, outHeader] -> do
      writeBeautyHeader inHeader outHeader
      putText "Success!"
    _ -> putText "gen-clean-binja-header path/to/original/binaryninjacore.h path/to/create/new/binaryninjacore.h"


  
  
