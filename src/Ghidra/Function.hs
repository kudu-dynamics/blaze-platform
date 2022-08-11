module Ghidra.Function where

import Ghidra.Prelude hiding (toList)

import Language.Clojure
import System.IO.Memoize (once)
import Foreign.JNI.Types (JObject)
import Ghidra.State (GhidraState(GhidraState))
import qualified Language.Java as Java
import Ghidra.Util (convertOpt)
import qualified Language.Clojure.Map as ClojureMap


requireModule :: IO ()
requireModule = unsafePerformIO . once $ do
  _ <- readEval "(require (quote [ghidra-clojure.function]))"
  return ()

newtype Function = Function JObject

fromAddr :: Address -> GhidraState -> IO Function
fromAddr addr (GhidraState gs) = do
  requireModule
  let fn = unsafeDupablePerformIO $ varQual "ghidra-clojure.function" "from-addr"
  addr' :: JObject <- coerce <$> Java.reflect (fromIntegral addr :: Int64)
  Function <$> invoke fn addr' gs

data GetFunctionsOptions = GetFunctionsOptions
  { defaults :: Maybe Bool
  , external :: Maybe Bool
  , local :: Maybe Bool
  , resolveThunks :: Maybe Bool
  , thunks :: Maybe Bool
  } deriving (Eq, Ord, Show, Generic)

defaultGetFunctionsOptions :: GetFunctionsOptions
defaultGetFunctionsOptions = GetFunctionsOptions
  { defaults = Nothing
  , external = Nothing
  , local = Nothing
  , resolveThunks = Nothing
  , thunks = Nothing
  }

prepGetFunctionsOpts :: GetFunctionsOptions -> IO [JObject]
prepGetFunctionsOpts opts = do
  a <- convertOpt "defaults" $ opts ^. #defaults
  b <- convertOpt "external" $ opts ^. #external
  c <- convertOpt "local" $ opts ^. #local
  d <- convertOpt "resolveThunks" $ opts ^. #resolveThunks
  e <- convertOpt "thunks" $ opts ^. #thunks
  return $ a <> b <> c <> d <> e

getFunctions' :: Maybe GetFunctionsOptions -> GhidraState -> IO [Function]
getFunctions' mOpts (GhidraState gs) = do
  requireModule
  let getFunctionsFn = unsafeDupablePerformIO $ varQual "ghidra-clojure.function" "get-functions"
  funcs <- case mOpts of
    Nothing -> invoke getFunctionsFn gs
    Just opts -> applyInvoke getFunctionsFn . (gs:) =<< prepGetFunctionsOpts opts
  vfuncs <- vec funcs
  putText "Here1"
  funcs' <- toList vfuncs
  putText "here2"
  -- funcs' <- vec funcs >>= toList
  return $ Function <$> funcs'

getFunctions :: GhidraState -> IO [Function]
getFunctions = getFunctions' Nothing
