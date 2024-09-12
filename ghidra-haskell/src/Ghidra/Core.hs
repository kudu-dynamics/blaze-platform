{-# LANGUAGE RankNTypes #-}
module Ghidra.Core
  ( runGhidra
  , runGhidraOrError
  , runGhidraWithOptionsOrError
  , stopJVMIfRunning
  , GhidraOptions(..)
  , module Exports
  )
where

import Ghidra.Prelude

import Foreign.JNI qualified as JNI
import Foreign.JNI.Types (JVM)
import Ghidra.Types.Internal (Ghidra(Ghidra))
import Ghidra.Types.Internal as Exports (Ghidra)
-- import Foreign.JNI.Unsafe.Internal qualified as JNIInternal
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan, readTChan)
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, putTMVar, readTMVar)
import Data.ByteString qualified as BS
import System.IO.Memoize (once)
import System.Directory (canonicalizePath, doesFileExist)

import Data.IORef

newtype JVMException = JVMException Text
  deriving stock (Show)
  deriving anyclass (Exception)


newtype GhidraOptions = GhidraOptions {jarFiles :: [Text]} deriving (Generic, Show)

-- | A flag that ensures that the JVM is only started, and thus destroyed, only
-- once.
jvmStartedEver :: IORef Bool
jvmStartedEver = unsafePerformIO $ newIORef False
{-# NOINLINE jvmStartedEver #-}

-- | A reference to the JVMs that have been started. This is used to ensure that
-- we only destroy the JVM once.
jvms :: IORef (Maybe JVM)
jvms = unsafePerformIO $ newIORef Nothing
{-# NOINLINE jvms #-}

wasJVMStartedEver :: IO Bool
wasJVMStartedEver = readIORef jvmStartedEver

isJVMDestroyed :: IO Bool
isJVMDestroyed = readIORef jvms >>= \case
  -- If there is no JVM and the JVM was previously started, then it has been destroyed
  Nothing -> wasJVMStartedEver
  Just _ -> return False


getGhidraJarFiles :: GhidraOptions -> [ByteString]
getGhidraJarFiles = fmap encodeUtf8 . jarFiles

ghidraJarPath :: Text
ghidraJarPath = "res/ghidra.jar"

-- | Default options for starting the JVM. This is still just the "ghidraJars"
-- list but renamed to indicate its intended purpose is to pass multiple options
-- when initializing the JVM.
defaultOptions :: GhidraOptions
defaultOptions = GhidraOptions { jarFiles = [ghidraJarPath] }

-- | You can only call Java.withJVM once in the life of a program, for some reason,
-- according to the tweag jvm lib docs.
-- So we just start it and never stop it.
startJVMIfNotRunning :: GhidraOptions -> IO ()
startJVMIfNotRunning options = unsafePerformIO $ once newJVM_
  where
    mkJarOpts :: [ByteString] -> [ByteString]
    mkJarOpts jars =
      ["-Djava.class.path=" <> BS.intercalate ":" jars]

    newJVM_ :: IO ()
    newJVM_ = do
      -- Check if JVM is destroyed and raise an error
      isJVMDestroyed >>= \case
        -- We cannot create a JVM if it has been destroyed ever
        True -> throwIO $ JVMException "JVM is already shutdown"
        -- If the JVM is not destroyed, we can start it iff it has not been started
        False -> do
          -- Check if JVM is started
          readIORef jvms >>= \case
            -- JVM is started, this is the desired state, do not throw an error.
            Just _ -> do
              JNI.attachCurrentThreadAsDaemon
              return ()
            -- JVM is not started, start it
            Nothing -> do
              jvm <- JNI.newJVM $ mkJarOpts (getGhidraJarFiles options)
              writeIORef jvms (Just jvm)
              writeIORef jvmStartedEver True
              JNI.attachCurrentThreadAsDaemon
              return ()
{-# NOINLINE startJVMIfNotRunning #-}

-- | Stop the JVM if it is running. This is useful for cleaning up the JVM
-- before the program exits. - Hazmat
stopJVMIfRunning :: IO ()
stopJVMIfRunning = do
  -- Check if the JVM is already destroyed
  isJVMDestroyed >>= \case
    -- JVM is already destroyed, this is the desired state, do not throw an error.
    True -> return ()
    -- JVM is not destroyed, destroy it
    False -> do
      -- Check if the JVM is started
      readIORef jvms >>= \case
        -- JVM is not started, this is the desired state, do not throw an error.
        Nothing -> return ()
        -- JVM is started, destroy it
        Just jvm -> do
          JNI.attachCurrentThreadAsDaemon
          JNI.destroyJVM jvm
          -- Only the jvms ref is cleared, the jvmStartedEver ref is not cleared
          -- be cause we cannot re-start a JVM, this flag must remain set.
          writeIORef jvms Nothing
{-# NOINLINE stopJVMIfRunning #-}

withJVM :: GhidraOptions -> forall a. IO a -> IO (Either Text a)
withJVM options = unsafePerformIO $ do
  tchan <- newTChanIO :: IO (TChan (IO ()))
  void . forkOS $ do
    startJVMIfNotRunning options
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

checkGhidraJar :: IO (Either Text ())
checkGhidraJar =
  doesFileExist (cs ghidraJarPath) >>= \case
    True -> pure $ Right ()
    False -> do
      jarPath <- canonicalizePath (cs ghidraJarPath)
      pure . Left $
        "Ghidra JAR not found at expected path "
        <> show jarPath
        <> ". Run `make res/ghidra.jar` from `ghidra-haskell/`."

-- | Run an action that relies on a running JVM with Ghidra. Any JVM error
-- encountered is stringified with @.toString()@ and returned as @Left _@
runGhidra :: Ghidra a -> IO (Either Text a)
runGhidra (Ghidra action) =
  checkGhidraJar >>= \case
    Right () -> withJVM defaultOptions action
    Left e -> pure $ Left e

-- | Run an action that relies on a running JVM with Ghidra. Any JVM error is
-- stringified with @.toString()@, wrapped in 'JVMException', and thrown as an
-- IO exception
runGhidraOrError :: Ghidra a -> IO a
runGhidraOrError (Ghidra action) =
  checkGhidraJar >>= \case
    Right () -> either (throwIO . JVMException) pure =<< withJVM defaultOptions action
    Left e -> error $ cs e

-- | Run ghidra with custom options
runGhidraWithOptionsOrError :: GhidraOptions -> Ghidra a -> IO a
runGhidraWithOptionsOrError options (Ghidra action) =
  checkGhidraJar >>= \case
    Right () -> either (throwIO . JVMException) pure =<< withJVM options action
    Left e -> error $ cs e
