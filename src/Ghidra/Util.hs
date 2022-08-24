module Ghidra.Util where

import Ghidra.Prelude hiding (force)

import Language.Clojure
import Foreign.JNI.Types (JObject)
import qualified Language.Java as Java
import Ghidra.Types (Iterator)
import Language.Java (J)
import qualified Foreign.JNI.Types as JNIT

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
maybeNull x = bool Nothing (Just x) $ isJNull x
