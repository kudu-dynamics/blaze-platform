module Ghidra.Util where

import Ghidra.Prelude hiding (force)

import Language.Clojure
import Foreign.JNI.Types (JObject)
import qualified Language.Java as Java
import qualified Ghidra.Types as J
import Ghidra.Types (Iterator)
import Language.Java (J(J))
import qualified Foreign.JNI.Types as JNIT
import qualified Foreign.JNI as JNI
import Foreign.Ptr (nullPtr)
import Foreign.ForeignPtr (withForeignPtr)


-- | returns a [key, val] list, to be used with invokeApply
convertOpt
  :: forall a. (Java.Reflect a)
  => Text
  -> Maybe a
  -> IO [JObject]
convertOpt _ Nothing = return []
convertOpt lbl (Just x) = do
  x' :: JObject <- coerce <$> Java.reflect x
  k <- keyword lbl
  return [k, x']

iteratorToList :: forall a. (Java.Coercible a, Coercible JObject a) => Iterator a -> IO [a]
iteratorToList it = do
  Java.call it "hasNext" >>= \case
    False -> return []
    True -> do
      x :: JObject <- Java.call it "next"
      (coerce x:) <$> iteratorToList it

isJNull :: J a -> Bool
isJNull x = x == JNIT.jnull

maybeNull :: J a -> Maybe (J a)
maybeNull x = bool (Just x) Nothing $ isJNull x

isJNull' :: J a -> IO Bool
isJNull' (J fptr) = withForeignPtr fptr $ return . (== nullPtr)

maybeNull' :: J a -> IO (Maybe (J a))
maybeNull' x = isJNull' x >>= return . bool (Just x) Nothing

-- | Catches call that failes with Java NullPointerException
maybeNullCall :: IO a -> IO (Maybe a)
maybeNullCall callAction = do
  jvm <- JNI.getJNIEnv
  catch (Just <$> JNI.throwIfException jvm callAction) (\(_ :: SomeException) -> return Nothing)

suppressOut :: IO a -> IO a
suppressOut action = do
  nullStream :: J.OutputStream <- Java.callStatic "java.io.OutputStream" "nullOutputStream"
  printStream :: J.PrintStream <- Java.new nullStream
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

