{-# LANGUAGE DataKinds #-}
module Ghidra.Function
  ( module Ghidra.Function
  ) where

import Ghidra.Prelude hiding (toList)

import Ghidra.State (GhidraState)
import qualified Ghidra.State as State
import qualified Language.Java as Java
import Ghidra.Util (maybeNull)
import qualified Ghidra.Types as J
import Ghidra.Types.Function (Function(Function), Parameter(Parameter))
import qualified Ghidra.Address as Addr
import qualified Foreign.JNI as JNI
import qualified Foreign.JNI.String as JString
import Ghidra.Util (iteratorToList)


fromAddr :: GhidraState -> J.Address -> IO (Maybe J.Function)
fromAddr gs addr = do
  listing <- State.getListing gs
  maybeNull <$> Java.call listing "getFunctionContaining" addr

functionIteratorToList :: J.FunctionIterator -> IO [J.Function]
functionIteratorToList = iteratorToList . coerce

getFuncs_ :: JString.String -> GhidraState -> IO [J.Function]
getFuncs_ methodName gs = do
  listing <- State.getListing gs
  Java.call listing methodName >>= functionIteratorToList
  
getExternalFunctions :: GhidraState -> IO [J.Function]
getExternalFunctions gs = do
  listing <- State.getListing gs
  Java.call listing "getExternalFunctions" >>= functionIteratorToList

getLocalFunctions :: GhidraState -> IO [J.Function]
getLocalFunctions gs = do
  listing <- State.getListing gs
  Java.call listing "getFunctions" True >>= functionIteratorToList

isThunk :: J.Function -> IO Bool
isThunk func = Java.call func "isThunk"

-- getThunk :: J.Function -> IO (Maybe Thunk)
-- getThunk func = isThunk func >>= return . bool Nothing (Just func)

resolveThunk :: J.Function -> IO J.Function
resolveThunk func = do
  isThunk func >>= \case
    False -> return func
    True -> Java.call func "getThunkedFunction" >>= JNI.newGlobalRef

hasDefaultName :: J.Function -> IO Bool
hasDefaultName func = do
  funcAddr <- J.toAddr func
  defFname :: Text <- Java.callStatic "ghidra.program.model.symbol.SymbolUtilities" "getDefaultFunctionName" funcAddr >>= Java.reify
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

getFunctions' :: GetFunctionsOptions -> GhidraState -> IO [J.Function]
getFunctions' opts gs = do
  externals <- bool (return []) (getExternalFunctions gs) $ opts ^. #includeExternalFuncs
  locals <- bool (return []) (getLocalFunctions gs) $ opts ^. #includeLocalFuncs
  let allFuncs = externals <> locals
  allFuncs' <- case opts ^. #excludeDefaultFuncs of
    False -> return allFuncs
    True -> filterM (fmap not . hasDefaultName) allFuncs
  case opts ^. #excludeThunks of
    False -> if opts ^. #resolveThunks
      then traverse resolveThunk allFuncs'
      else return allFuncs'
    True -> filterM (fmap not . isThunk) allFuncs'

getFunctions :: GhidraState -> IO [J.Function]
getFunctions = getFunctions' defaultGetFunctionsOptions

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

-- | Gets the High version of the function, based off of decompilation analysis.
-- This is expensive.
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

hasVarArgs :: J.Function -> IO Bool
hasVarArgs fn = Java.call fn "hasVarArgs"

mkFunction :: J.Function -> IO Function
mkFunction fn = Function fn <$> getAddress fn

mkParameter :: J.Parameter -> IO Parameter
mkParameter p = do
  ordIndex :: Int32 <- Java.call p "getOrdinal"
  isAuto <- Java.call p "isAutoParameter"
  mname :: Maybe Text <- (maybeNull <$> Java.call p "getName")
    >>= traverse Java.reify
  let name = fromMaybe ("arg" <> show ordIndex) mname
  return $ Parameter p (fromIntegral ordIndex) isAuto name

getParams :: J.Function -> IO [Parameter]
getParams fn =
  Java.call fn "getParameters" >>= Java.reify >>= traverse mkParameter
