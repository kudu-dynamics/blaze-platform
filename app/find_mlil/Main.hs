module Main where

import Blaze.Prelude

import Blaze.Util.MLIL
import qualified Data.Text as Text

main :: IO ()
main = getArgs >>= \case
  [opName, binPath] -> do
    xs <- getInstructionsWithOpByName (Text.pack opName) binPath
    mapM_ p xs
    putText "[finished]"
    exitSuccess
    where
      p (fn, ix) = putText $ fn <> " " <> show ix
  _ -> putText "find_mlil [op name] [binary path]"

