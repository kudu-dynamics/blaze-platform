module Ghidra.Core where

import Prelude

import Language.Clojure
import System.IO.Unsafe (unsafeDupablePerformIO)
import Foreign (Int64)
import GHC.Generics (Generic)
import Foreign.JNI.Types (JObject)
import qualified Data.Text as Text
import qualified Language.Java as Java
import Data.ByteString (ByteString)
import qualified Ghidra.State as State
import qualified Language.Clojure.Map as Map

ghidraJars :: [ByteString]
ghidraJars =
  [ "res/ghidra-clojure-1.0.9-standalone.jar"
  , "res/ghidra-10.1.4.jar"
  ]

runGhidra :: IO a -> IO a
runGhidra = runClojure ghidraJars

testt :: IO ()
testt = runGhidra $ do
  db <- State.openDatabase "/tmp/kudu/assembly/contrived4"
  print db

testMap :: IO ()
testMap = runGhidra $ do
  k1 <- keyword "hey"
  k2 <- keyword "there"
  m <- Map.fromList [(k1, 5 :: Int64), (k2, 88 :: Int64)]
  printObj m
