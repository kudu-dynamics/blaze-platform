{- HLINT ignore "Redundant bracket" -}

module Ghidra.Util where

import Ghidra.Prelude hiding (force)

import Foreign.JNI.Types (JObject)
import qualified Language.Java as Java
import qualified Ghidra.Types as J
import Ghidra.Types (Iterator)
import Language.Java (J(J))
import qualified Foreign.JNI.Types as JNI
import qualified Foreign.JNI as JNI
import Foreign.Ptr (nullPtr)
import Foreign.ForeignPtr (withForeignPtr)


iteratorToList :: forall a. (Java.Coercible a, Coercible JObject a) => Iterator a -> IO [a]
iteratorToList it = do
  Java.call it "hasNext" >>= \case
    False -> return []
    True -> do
      x :: JObject <- Java.call it "next"
      (coerce x:) <$> iteratorToList it

isJNull :: J a -> Bool
isJNull x = x == JNI.jnull

maybeNull :: J a -> Maybe (J a)
maybeNull x = bool (Just x) Nothing $ isJNull x

isJNull' :: J a -> IO Bool
isJNull' (J fptr) = withForeignPtr fptr $ return . (== nullPtr)

maybeNull' :: J a -> IO (Maybe (J a))
maybeNull' x = bool (Just x) Nothing <$> isJNull' x

-- | Catches any Java NullPointerException and returns 'Nothing' instead
maybeNullCall :: IO a -> IO (Maybe a)
maybeNullCall callAction = do
  jvm <- JNI.getJNIEnv
  -- Try to run action, but catch any JVM NPEs. However, some internal
  -- inline-java code might have lifted some NPE into a Haskell-side
  -- JNI.NullPointerException, so check for that first.
  (Just <$> JNI.throwIfException jvm callAction)
  `catches`
    [ Handler (\(_ :: JNI.NullPointerException) -> pure Nothing)
    , Handler (\ex@(JNI.JVMException jex) -> do
        npeClass <- JNI.findClass $ JNI.referenceTypeName (JNI.SClass "java.lang.NullPointerException")
        JNI.isInstanceOf jex npeClass >>= \case
          True -> pure Nothing
          False -> throwIO ex)
    ]

tryJVM :: IO a -> IO (Either Text a)
tryJVM action = do
  try action >>= \case
    Left (e :: JNI.JVMException) -> Left <$> JNI.showException e
    Right a -> pure $ Right a

suppressOut :: IO a -> IO a
suppressOut action = do
  bracket quietStreams setStreams $ const action
  where
    quietStreams :: IO (J.PrintStream, J.PrintStream)
    quietStreams = do
      nullStream :: J.OutputStream <- Java.callStatic "java.io.OutputStream" "nullOutputStream"
      nullPrintStream :: J.PrintStream <- Java.new nullStream
      r <- (,) <$> Java.getStaticField "java.lang.System" "out" <*> Java.getStaticField "java.lang.System" "err"
      setStreams (nullPrintStream, nullPrintStream)
      return r

    setStreams :: (J.PrintStream, J.PrintStream) -> IO ()
    setStreams (out, err) = do
      _ :: () <- Java.callStatic "java.lang.System" "setOut" out
      _ :: () <- Java.callStatic "java.lang.System" "setErr" err
      return ()

