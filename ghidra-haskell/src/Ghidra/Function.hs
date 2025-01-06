{-# LANGUAGE DataKinds #-}
module Ghidra.Function
  ( module Ghidra.Function
  ) where

import Ghidra.Prelude hiding (toList)

import Ghidra.State (GhidraState)
import qualified Ghidra.State as State
import qualified Language.Java as Java
import Ghidra.Util (iteratorToList, maybeNull)
import qualified Ghidra.Types as J
import Ghidra.Types.Function (Function(Function), Parameter(Parameter))
import Ghidra.Types.Internal (Ghidra, runIO)
import qualified Ghidra.Address as Addr
import qualified Foreign.JNI as JNI
import qualified Foreign.JNI.String as JString


fromAddr :: GhidraState -> J.Address -> Ghidra (Maybe J.Function)
fromAddr gs addr = do
  listing <- State.getListing gs
  maybeNull <$> runIO (Java.call listing "getFunctionContaining" addr)

functionIteratorToList :: J.FunctionIterator -> Ghidra [J.Function]
functionIteratorToList = iteratorToList . coerce

getFuncs_ :: JString.String -> GhidraState -> Ghidra [J.Function]
getFuncs_ methodName gs = do
  listing <- State.getListing gs
  runIO (Java.call listing methodName) >>= functionIteratorToList
  
getExternalFunctions :: GhidraState -> Ghidra [J.Function]
getExternalFunctions gs = do
  listing <- State.getListing gs
  runIO (Java.call listing "getExternalFunctions") >>= functionIteratorToList

getLocalFunctions :: GhidraState -> Ghidra [J.Function]
getLocalFunctions gs = do
  listing <- State.getListing gs
  runIO (Java.call listing "getFunctions" True) >>= functionIteratorToList

isThunk :: J.Function -> Ghidra Bool
isThunk func = runIO $ Java.call func "isThunk"

-- | Only safe if `isThunk func` is True.
unsafeGetThunkedFunction :: Bool -> J.Function -> Ghidra J.Function
unsafeGetThunkedFunction recursive func = runIO $ Java.call func "getThunkedFunction" recursive

getThunkedFunction :: Bool -> J.Function -> Ghidra (Maybe J.Function)
getThunkedFunction recursive func = isThunk func >>= \case
  False -> return Nothing
  True -> Just <$> unsafeGetThunkedFunction recursive func

resolveThunk :: J.Function -> Ghidra J.Function
resolveThunk func = do
  isThunk func >>= \case
    False -> return func
    True -> runIO $ Java.call func "getThunkedFunction" >>= JNI.newGlobalRef

hasDefaultName :: J.Function -> Ghidra Bool
hasDefaultName func = do
  funcAddr <- J.toAddr func
  defFname :: Text <- runIO $ Java.callStatic "ghidra.program.model.symbol.SymbolUtilities" "getDefaultFunctionName" funcAddr >>= Java.reify
  fname <- getName func
  return $ fname == defFname

data GetFunctionsOptions = GetFunctionsOptions
  { includeExternalFuncs :: Bool
  , includeLocalFuncs :: Bool
  , excludeDefaultFuncs :: Bool
  , excludeThunks :: Bool
  , resolveThunks :: Bool
  } deriving (Eq, Ord, Show, Generic)

defaultGetFunctionsOptions :: GetFunctionsOptions
defaultGetFunctionsOptions = GetFunctionsOptions
  { includeExternalFuncs = True
  , includeLocalFuncs = True
  , excludeDefaultFuncs = False
  , excludeThunks = False
  , resolveThunks = False
  }

getFunctions' :: GetFunctionsOptions -> GhidraState -> Ghidra [J.Function]
getFunctions' opts gs = do
  externals <- bool (return []) (getExternalFunctions gs) $ opts ^. #includeExternalFuncs
  locals <- bool (return []) (getLocalFunctions gs) $ opts ^. #includeLocalFuncs
  let allFuncs = externals <> locals
  allFuncs' <-
    if opts ^. #excludeDefaultFuncs then
      filterM (fmap not . hasDefaultName) allFuncs
    else
      return allFuncs
  if opts ^. #excludeThunks then
    filterM (fmap not . isThunk) allFuncs'
  else if opts ^. #resolveThunks then
    traverse resolveThunk allFuncs'
  else
    return allFuncs'

getFunctions :: GhidraState -> Ghidra [J.Function]
getFunctions = getFunctions' defaultGetFunctionsOptions

-- | This is expensive and should be performed only once per function.
decompileFunction :: GhidraState -> J.Function -> Ghidra J.DecompilerResults
decompileFunction gs fn = do
  flatDecAPI <- State.getFlatDecompilerAPI gs
  _ :: () <- runIO $ Java.call flatDecAPI "initialize"
  ifc :: J.DecompInterface <- runIO $ Java.call flatDecAPI "getDecompiler"
  mon <- State.getTaskMonitor gs
  res :: J.DecompilerResults <- runIO $ Java.call ifc "decompileFunction" fn (0 :: Int32) mon >>= JNI.newGlobalRef
  finished <- runIO $ Java.call res "decompileCompleted"
  if finished
    then return res
    else error "Could not decompile function"

-- | Gets the High version of the function, based off of decompilation analysis.
-- This is expensive.
getHighFunction :: GhidraState -> J.Function -> Ghidra J.HighFunction
getHighFunction gs fn = do
  r <- decompileFunction gs fn
  runIO $ Java.call r "getHighFunction" >>= JNI.newGlobalRef

getLowFunction :: J.HighFunction -> Ghidra J.Function
getLowFunction fn = runIO $ Java.call fn "getFunction" >>= JNI.newGlobalRef

getName :: J.Function -> Ghidra Text
getName fn = runIO $ Java.call fn "getName" >>= JNI.newGlobalRef >>= Java.reify

getAddress :: J.Function -> Ghidra Addr.Address
getAddress = Addr.mkAddress <=< J.toAddr

hasVarArgs :: J.Function -> Ghidra Bool
hasVarArgs fn = runIO $ Java.call fn "hasVarArgs"

isInline :: J.Function -> Ghidra Bool
isInline fn = runIO $ Java.call fn "isInline"

isExternal :: J.Function -> Ghidra Bool
isExternal fn = runIO $ Java.call fn "isExternal"

mkFunction :: J.Function -> Ghidra Function
mkFunction fn = Function fn <$> getAddress fn

mkParameter :: J.Parameter -> Ghidra Parameter
mkParameter p = runIO $ do
  ordIndex :: Int32 <- Java.call p "getOrdinal"
  isAuto <- Java.call p "isAutoParameter"
  mname :: Maybe Text <- traverse Java.reify . maybeNull =<< Java.call p "getName"
  let name = fromMaybe ("arg" <> show ordIndex) mname
  return $ Parameter p (fromIntegral ordIndex) isAuto name

getParams :: J.Function -> Ghidra [Parameter]
getParams fn =
  runIO (Java.call fn "getParameters" >>= Java.reify) >>= traverse mkParameter

getExternalLocation :: J.Function -> Ghidra J.ExternalLocationDB
getExternalLocation fn = runIO $ coerce (Java.call fn "getExternalLocation" :: IO J.ExternalLocation)


