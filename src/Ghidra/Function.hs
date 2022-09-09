{-# LANGUAGE DataKinds #-}
module Ghidra.Function
  ( module Ghidra.Function
  ) where

import Ghidra.Prelude hiding (toList)

import Language.Clojure
import System.IO.Memoize (once)
import Foreign.JNI.Types (JObject)
import Ghidra.State (GhidraState(GhidraState))
import qualified Ghidra.State as State
import qualified Language.Java as Java
import Ghidra.Util (convertOpt, maybeNull)
import qualified Ghidra.Types as J
import Ghidra.Types.Function (Function(Function))
import qualified Ghidra.Address as Addr
import qualified Foreign.JNI as JNI


requireModule :: IO ()
requireModule = unsafePerformIO . once $ do
  _ <- readEval "(require (quote [ghidra-clojure.function]))"
  return ()

fromAddr :: GhidraState -> J.Address -> IO (Maybe J.Function)
fromAddr gs addr = do
  listing <- State.getListing gs
  maybeNull <$> Java.call listing "getFunctionContaining" addr

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

getFunctions' :: Maybe GetFunctionsOptions -> GhidraState -> IO [J.Function]
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

getFunctions :: GhidraState -> IO [J.Function]
getFunctions = getFunctions' Nothing

-- | This is expensive and should be performed only once per function.
decompileFunction :: GhidraState -> J.Function -> IO J.DecompilerResults
decompileFunction gs fn = do
  flatDecAPI <- State.getFlatDecompilerAPI gs
  _ :: () <-  Java.call flatDecAPI "initialize"
  ifc :: J.DecompInterface <- Java.call flatDecAPI "getDecompiler"
  mon <- State.getTaskMonitor gs
  res :: J.DecompilerResults <- Java.call ifc "decompileFunction" fn (0 :: Int32) mon >>= JNI.newGlobalRef
  finished <- Java.call res "decompileCompleted"
  if finished
    then return res
    else error "Could not decompile function"
  
getHighFunction :: GhidraState -> J.Function -> IO J.HighFunction
getHighFunction gs fn = do
  r <- decompileFunction gs fn
  Java.call r "getHighFunction" >>= JNI.newGlobalRef

getLowFunction :: J.HighFunction -> IO J.Function
getLowFunction fn = Java.call fn "getFunction" >>= JNI.newGlobalRef

getName :: J.Function -> IO Text
getName fn = Java.call fn "getName" >>= JNI.newGlobalRef >>= Java.reify

getAddress :: J.Function -> IO Addr.Address
getAddress = Addr.mkAddress <=< J.toAddr

mkFunction :: J.Function -> IO Function
mkFunction fn = Function fn <$> getAddress fn
