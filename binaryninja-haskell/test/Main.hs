module Main where

import Prelude (IO)

import qualified Binja.Core as BN

import qualified Spec

main :: IO ()
main = do
  Spec.main
  BN.shutdown
