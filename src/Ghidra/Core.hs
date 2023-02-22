module Ghidra.Core
  ( runGhidra
  , runGhidraOrError
  )
where

import Ghidra.Prelude

import qualified Foreign.JNI as JNI
import qualified Data.ByteString as BS
import System.IO.Memoize (once)

newtype JVMException = JVMException Text
  deriving stock (Show)
  deriving anyclass (Exception)

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
-- So we just start it and never stop it.
attachJVM :: IO ()
attachJVM = unsafePerformIO $ once newJVM_
  where
    mkJarOpts :: [ByteString] -> [ByteString]
    mkJarOpts jars =
      [ "-Djava.class.path=" <> BS.intercalate ":" jars ]

    newJVM_ :: IO ()
    newJVM_ = void . JNI.newJVM $ mkJarOpts ghidraJars
{-# NOINLINE attachJVM #-}

withJVM :: forall a. IO a -> IO (Either Text a)
withJVM action = do
  res :: Either JNI.JVMException a <- try (attachJVM >> action)
  case res of
    Left e -> Left <$> JNI.showException e
    Right a -> pure $ Right a

-- | Run an action that relies on a running JVM with Ghidra. Any JVM error
-- encountered is stringified with @.toString()@ and returned as @Left _@
runGhidra :: IO a -> IO (Either Text a)
runGhidra = withJVM

-- | Run an action that relies on a running JVM with Ghidra. Any JVM error is
-- stringified with @.toString()@, wrapped in 'JVMException', and thrown as an
-- IO exception
runGhidraOrError :: IO a -> IO a
runGhidraOrError a = either (throwIO . JVMException) pure =<< withJVM a
