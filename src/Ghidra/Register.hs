module Ghidra.Register
  ( module Ghidra.Register
  , module Ghidra.Types.Register
  ) where

import Ghidra.Prelude

import Language.Java as Java
import qualified Ghidra.Types as J
import Ghidra.Types.Register

getName :: J.Register -> IO Text
getName reg = do
  name :: Text <- Java.call reg "getName"
                  >>= Java.reify
  return name

getBitLength :: J.Register -> IO Bits
getBitLength reg = do
  size :: Int32 <- Java.call reg "getBitLength"
  return (Bits $ fromIntegral size)
