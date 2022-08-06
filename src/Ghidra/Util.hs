module Ghidra.Util where

import Ghidra.Prelude hiding (force)

import Language.Clojure
import Foreign.JNI.Types (JObject)
import qualified Language.Java as Java


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
