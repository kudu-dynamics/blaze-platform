module Ghidra.Core
  ( runGhidra
  , runGhidraOrError
  , module Exports
  )
where

import Ghidra.Prelude

import Ghidra.Types.Internal as Exports (Ghidra)
import Ghidra.Types.Internal (Ghidra(Ghidra))
import qualified Foreign.JNI as JNI
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan, readTChan)
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, putTMVar, readTMVar)
import qualified Data.ByteString as BS
import System.IO.Memoize (once)


newtype JVMException = JVMException Text
  deriving stock (Show)
  deriving anyclass (Exception)

ghidraJars :: [ByteString]
ghidraJars =
  [ "res/ghidra.jar"
  ]

-- | You can only call Java.withJVM once in the life of a program, for some reason,
-- according to the tweag jvm lib docs.
-- So we just start it and never stop it.
startJVMIfNotRunning :: IO ()
startJVMIfNotRunning = unsafePerformIO $ once newJVM_
  where
    mkJarOpts :: [ByteString] -> [ByteString]
    mkJarOpts jars =
      [ "-Djava.class.path=" <> BS.intercalate ":" jars ]

    newJVM_ :: IO ()
    newJVM_ = do
      void . JNI.newJVM $ mkJarOpts ghidraJars
      JNI.attachCurrentThreadAsDaemon
{-# NOINLINE startJVMIfNotRunning #-}

withJVM :: forall a. IO a -> IO (Either Text a)
withJVM = unsafePerformIO $ do
  tchan <- newTChanIO :: IO (TChan (IO ()))
  void . forkOS $ do
    startJVMIfNotRunning
    forever $ do
      action <- atomically (readTChan tchan)
      try action >>= \case
        Left (_ :: SomeException) -> putText "Action failed"
        Right _ -> return ()
  return $ runnerFunc tchan
  where
    runnerFunc :: TChan (IO ()) -> IO a -> IO (Either Text a)
    runnerFunc tchan action = do
      resultVar <- newEmptyTMVarIO
      atomically . writeTChan tchan $ do
        res :: Either JNI.JVMException a <- try action
        res' <- case res of
          Left e -> Left <$> JNI.showException e
          Right a -> pure $ Right a
        atomically . putTMVar resultVar $ res'
      atomically $ readTMVar resultVar
{-# NOINLINE withJVM #-}

-- | Run an action that relies on a running JVM with Ghidra. Any JVM error
-- encountered is stringified with @.toString()@ and returned as @Left _@
runGhidra :: Ghidra a -> IO (Either Text a)
runGhidra (Ghidra action) = withJVM action

-- | Run an action that relies on a running JVM with Ghidra. Any JVM error is
-- stringified with @.toString()@, wrapped in 'JVMException', and thrown as an
-- IO exception
runGhidraOrError :: Ghidra a -> IO a
runGhidraOrError (Ghidra action) = either (throwIO . JVMException) pure =<< withJVM action
