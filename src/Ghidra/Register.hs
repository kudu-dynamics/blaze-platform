module Ghidra.Register
  ( module Ghidra.Register
  , module Ghidra.Types.Register
  ) where

import Ghidra.Prelude

import Language.Java as Java
import qualified Ghidra.Types as J
import Ghidra.Types.Internal (Ghidra, runIO)
import Ghidra.Types.Register


getName :: J.Register -> Ghidra Text
getName reg = do
  name :: Text <- runIO $ Java.call reg "getName"
                  >>= Java.reify
  return name

getBitLength :: J.Register -> Ghidra Bits
getBitLength reg = do
  size :: Int32 <- runIO $ Java.call reg "getBitLength"
  return (Bits $ fromIntegral size)
