{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Clojure.Map where

import Prelude

import Control.Concurrent
import Control.Monad.Extra (concatMapM)
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Foreign (Int64)
import qualified Foreign.JNI.Types as JNIT
import Foreign.JNI.Types (JObject)
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Language.Java as Java
import Language.Java (J, VariadicIO)
import qualified Data.ByteString as BS
import Language.Clojure.Core
import qualified Data.Coerce as Coerce
import Data.HashMap.Strict (HashMap)


-- | Create Clojure map
fromList
  :: forall a b.
     ( Java.Reflect a
     , Java.Reflect b
     )
  => [(a, b)]
  -> IO JObject
fromList xs = do
  kvals <- reflectTupleList xs
  let hmapFn = unsafeDupablePerformIO $ varQual "clojure.core" "hash-map"
  apply hmapFn kvals
  where
    reflectTupleList :: [(a, b)] -> IO [JObject]
    reflectTupleList = concatMapM reflectTuple

    reflectTuple :: (a, b) -> IO [JObject]
    reflectTuple (x, y) = do
      x' :: JObject <- coerce <$> Java.reflect x
      y' :: JObject <- coerce <$> Java.reflect y
      return [x', y']
