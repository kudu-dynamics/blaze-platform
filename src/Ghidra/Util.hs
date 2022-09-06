module Ghidra.Util where

import Ghidra.Prelude hiding (force)

import Language.Clojure
import Foreign.JNI.Types (JObject)
import qualified Language.Java as Java
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
