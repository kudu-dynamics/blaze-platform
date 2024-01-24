module Ghidra.CoreSpec where

import Ghidra.Prelude

import Ghidra.Core
import qualified Ghidra.State as State
import qualified Ghidra.Function as Function

import Control.Concurrent.Async (replicateConcurrently_)
import Test.Hspec


a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1.gzf"

spec :: Spec
spec = describe "Ghidra.Core" $ do
  gs <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ a1Bin >>! State.analyze
    return gs

  context "concurrency" $ do
    let action = replicateConcurrently_ 10 $ do
          void . runGhidraOrError $ Function.getFunctions gs
    it "should be able to handle concurrent access to same GhidraState" $ do
     action `shouldReturn` ()
