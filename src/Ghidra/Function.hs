{-# LANGUAGE DataKinds #-}
module Ghidra.Function
  ( module Ghidra.Function
  , Function
  , HighFunction
  ) where

import Ghidra.Prelude hiding (toList)

import Language.Clojure
import System.IO.Memoize (once)
import Foreign.JNI.Types (JObject)
import Ghidra.State (GhidraState(GhidraState))
import qualified Ghidra.State as State
import qualified Language.Java as Java
import Ghidra.Util (convertOpt)
import Ghidra.Types
import qualified Ghidra.Address as Addr

requireModule :: IO ()
requireModule = unsafePerformIO . once $ do
  _ <- readEval "(require (quote [ghidra-clojure.function]))"
  return ()

fromAddr :: GhidraState -> Address -> IO (Maybe Function)
fromAddr (GhidraState gs) addr = do
  requireModule
  let fn = unsafeDupablePerformIO $ varQual "ghidra-clojure.function" "from-addr"
  r <- invoke fn (coerce addr :: JObject) gs
  isNil' r >>= return . bool (Just . coerce $ r) Nothing

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
    Just opts -> do
      -- TODO: Function opts seem not to work
      opts' <- prepGetFunctionsOpts opts
      mapM_ (\x -> do
                toString x >>= putText
                showClass x >>= putText
            ) opts'
      applyInvoke getFunctionsFn $ gs : opts'
  funcs' <- vec funcs >>= toList
  return $ coerce <$> funcs'

getFunctions :: GhidraState -> IO [Function]
getFunctions = getFunctions' Nothing

-- | This is expensive and should be performed only once per function.
decompileFunction :: GhidraState -> Function -> IO DecompilerResults
decompileFunction gs fn = do
  flatDecAPI <- State.getFlatDecompilerAPI gs
  _ :: () <-  Java.call flatDecAPI "initialize"
  ifc :: DecompInterface <- Java.call flatDecAPI "getDecompiler"
  mon <- State.getTaskMonitor gs
  res :: DecompilerResults <- Java.call ifc "decompileFunction" fn (0 :: Int32) mon
  finished <- Java.call res "decompileCompleted"
  if finished
    then return res
    else error "Could not decompile function"
  
getHighFunction :: GhidraState -> Function -> IO HighFunction
getHighFunction gs fn = do
  r <- decompileFunction gs fn
  Java.call r "getHighFunction"

getName :: Function -> IO Text
getName fn = Java.call fn "getName" >>= Java.reify

getAddress :: Function -> IO Addr.Address
getAddress = Addr.mkAddress <=< toAddr
