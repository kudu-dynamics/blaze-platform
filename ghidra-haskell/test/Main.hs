module Main where

import Prelude (IO)

import Ghidra.Core (stopJVMIfRunning)

import qualified Spec

main :: IO ()
main = do
  Spec.main
  stopJVMIfRunning
