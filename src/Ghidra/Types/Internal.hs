module Ghidra.Types.Internal where

import Ghidra.Prelude


newtype Ghidra a = Ghidra { _runGhidra :: IO a }
  deriving newtype (Functor, Monad, Applicative, MonadFail)

runIO :: IO a -> Ghidra a
runIO = Ghidra
