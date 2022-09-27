module Ghidra.Core where

import Ghidra.Prelude

import qualified Foreign.JNI as JNI
import qualified Data.ByteString as BS
import System.IO.Memoize (once)


-- ghidraJars :: [ByteString]
-- ghidraJars =
--   [ "res/ghidra-10.1.4.jar"
--   ]

ghidraJars :: [ByteString]
ghidraJars =
  [ "res/ghidra.jar"
  ]

-- | You can only call Java.withJVM once in the life of a program, for some reason,
-- according to the tweag jvm lib docs.
-- So we just start it and never stop it. If the opts change, we throw an error.
attachJVM :: IO ()
attachJVM = unsafePerformIO $ once newJVM_
  where
    mkJarOpts :: [ByteString] -> [ByteString]
    mkJarOpts jars =
      [ "-Djava.class.path=" <> BS.intercalate ":" jars ]

    newJVM_ :: IO ()
    newJVM_ = void . JNI.newJVM $ mkJarOpts ghidraJars

withJVM :: IO a -> IO a
withJVM action = do
  attachJVM
  action

runGhidra :: IO a -> IO a
runGhidra = withJVM
