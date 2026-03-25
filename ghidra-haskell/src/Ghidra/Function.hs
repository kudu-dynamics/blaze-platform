{-# LANGUAGE DataKinds #-}
module Ghidra.Function
  ( module Ghidra.Function
  ) where

import Ghidra.Prelude hiding (toList)

import qualified Ghidra.Program as Program
import Ghidra.State (GhidraState)
import qualified Ghidra.State as State
import qualified Language.Java as Java
import Ghidra.Util (iteratorToList, maybeNull)
import qualified Ghidra.Types as J
import Ghidra.Types.Function (Function(Function), HighParameter(HighParameter), Parameter(Parameter))
import Ghidra.Types.Internal (Ghidra, runIO)
import Ghidra.Types.Pcode (SimplificationStyle(Decompile))
import qualified Ghidra.Address as Addr
import qualified Foreign.JNI as JNI
import qualified Foreign.JNI.String as JString
import Ghidra.Clang (buildClangAST, ClangAST, ClangNode)
import qualified Data.Text as Text


fromAddr :: J.ProgramDB -> J.Address -> Ghidra (Maybe J.Function)
fromAddr prg addr = do
  listing <- State.getListing prg
  maybeNull <$> runIO (Java.call listing "getFunctionContaining" addr)

getFunctionAt :: J.ProgramDB -> J.Address -> Ghidra (Maybe J.Function)
getFunctionAt prg addr = do
  fm <- Program.getFunctionManager prg
  maybeNull <$> runIO (Java.call fm "getFunctionAt" addr)

functionIteratorToList :: J.FunctionIterator -> Ghidra [J.Function]
functionIteratorToList = iteratorToList . coerce

-- | Get all functions that call the given function.
-- For thunk targets (like externs), follows through thunk addresses
-- to find actual callers.
getCallingFunctions :: GhidraState -> J.Function -> Ghidra [J.Function]
getCallingFunctions gs fn = do
  mon <- State.getTaskMonitor gs
  -- First try direct callers
  direct <- getCallingFunctions_ mon fn
  if not (null direct)
    then return direct
    else do
      -- If no direct callers, check if this function has thunks pointing to it
      -- (common for extern functions — callers reference the thunk, not the extern)
      thunkAddrs <- getFunctionThunkAddresses fn
      thunkCallers <- forM thunkAddrs $ \thunkAddr -> do
        mthunk <- fromAddr (gs ^. #program) thunkAddr
        case mthunk of
          Nothing -> return []
          Just thunk -> getCallingFunctions_ mon thunk
      return $ concat thunkCallers

getCallingFunctions_ :: J.TaskMonitor -> J.Function -> Ghidra [J.Function]
getCallingFunctions_ mon fn = do
  jset :: J.Set J.Function <- runIO $ Java.call fn "getCallingFunctions" mon >>= JNI.newGlobalRef
  jiter :: J.Iterator J.Function <- runIO $ Java.call jset "iterator" >>= JNI.newGlobalRef
  iteratorToList jiter


getFuncs_ :: JString.String -> J.ProgramDB -> Ghidra [J.Function]
getFuncs_ methodName prg = do
  listing <- State.getListing prg
  runIO (Java.call listing methodName) >>= functionIteratorToList

getExternalFunctions :: J.ProgramDB -> Ghidra [J.Function]
getExternalFunctions prg = do
  listing <- State.getListing prg
  runIO (Java.call listing "getExternalFunctions") >>= functionIteratorToList

getLocalFunctions :: J.ProgramDB -> Ghidra [J.Function]
getLocalFunctions prg = do
  listing <- State.getListing prg
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
    True -> runIO $ Java.call func "getThunkedFunction" True >>= JNI.newGlobalRef

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

getFunctions' :: GetFunctionsOptions -> J.ProgramDB -> Ghidra [J.Function]
getFunctions' opts prg = do
  externals <- bool (return []) (getExternalFunctions prg) $ opts ^. #includeExternalFuncs
  locals <- bool (return []) (getLocalFunctions prg) $ opts ^. #includeLocalFuncs
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

getFunctions :: J.ProgramDB -> Ghidra [J.Function]
getFunctions = getFunctions' defaultGetFunctionsOptions

-- | Get or create a properly initialized DecompInterface.
-- Unlike FlatDecompilerAPI.initialize(), this checks whether openProgram
-- actually succeeded (Ghidra 12 can fail silently, leaving internal state null).
getOrCreateDecompInterface :: GhidraState -> Ghidra J.DecompInterface
getOrCreateDecompInterface gs = do
  flatDecAPI <- State.getFlatDecompilerAPI gs
  ifc :: J.DecompInterface <- runIO $ Java.call flatDecAPI "getDecompiler"
  case maybeNull ifc of
    Just i -> return i
    Nothing -> do
      newIfc :: J.DecompInterface <- runIO $ Java.new >>= JNI.newGlobalRef
      prg <- State.getProgram gs
      opened :: Bool <- runIO $ Java.call newIfc "openProgram" (coerce prg :: J.Program)
      unless opened $
        error "Decompiler failed to open program. Check that ghidra.jar contains the decompiler binary for this architecture."
      return newIfc

getClangAST :: GhidraState -> J.Function -> Ghidra (ClangAST ClangNode)
getClangAST gs fn = do
  ifc <- getOrCreateDecompInterface gs
  mon <- State.getTaskMonitor gs
  res :: J.DecompilerResults <- runIO $ Java.call ifc "decompileFunction" fn (0 :: Int32) mon >>= JNI.newGlobalRef
  tokenGroup :: J.ClangTokenGroup <- runIO $ Java.call res "getCCodeMarkup"
  let rootNode :: J.ClangNode = coerce tokenGroup
  buildClangAST (gs ^. #program) rootNode

simplStyleStr :: SimplificationStyle -> Text
simplStyleStr = Text.toLower . Text.pack . show

-- | This is expensive and should be performed only once per function.
decompileFunction_ :: GhidraState -> J.Function -> SimplificationStyle -> Ghidra J.DecompilerResults
decompileFunction_ gs fn st = do
  ifc <- getOrCreateDecompInterface gs
  let simpStyle = simplStyleStr st
  _ :: Bool <- runIO $ Java.call ifc "setSimplificationStyle" =<< Java.reflect simpStyle
  mon <- State.getTaskMonitor gs
  res :: J.DecompilerResults <- runIO $ Java.call ifc "decompileFunction" fn (0 :: Int32) mon >>= JNI.newGlobalRef

  finished <- runIO $ Java.call res "decompileCompleted"
  if finished
    then return res
    else error "Could not decompile function"

decompileFunction :: GhidraState -> J.Function -> Ghidra J.DecompilerResults
decompileFunction gs fn = decompileFunction_ gs fn Decompile

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

getProto :: J.HighFunction -> Ghidra J.FunctionPrototype
getProto fn = runIO $ Java.call fn "getFunctionPrototype" >>= JNI.newGlobalRef

isConstructor :: J.FunctionPrototype -> Ghidra Bool
isConstructor proto = runIO $ Java.call proto "isConstructor"

isDestructor :: J.FunctionPrototype -> Ghidra Bool
isDestructor proto = runIO $ Java.call proto "isDenstructor"

isInline :: J.FunctionPrototype -> Ghidra Bool
isInline proto = runIO $ Java.call proto "isInline"

isVarArg :: J.FunctionPrototype -> Ghidra Bool
isVarArg proto = runIO $ Java.call proto "isVarArg"

isExternal :: J.Function -> Ghidra Bool
isExternal fn = runIO $ Java.call fn "isExternal"

getLibraryName :: J.ExternalLocationDB -> Ghidra (Maybe Text)
getLibraryName eloc = runIO $ traverse Java.reify . maybeNull =<< Java.call eloc "getLibraryName"

mkFunction :: J.Function -> Ghidra Function
mkFunction fn = Function fn <$> getAddress fn

mkParameter :: J.Parameter -> Ghidra Parameter
mkParameter p = runIO $ do
  ordIndex :: Int32 <- Java.call p "getOrdinal"
  isAuto <- Java.call p "isAutoParameter"
  mname :: Maybe Text <- traverse Java.reify . maybeNull =<< Java.call p "getName"
  let name = fromMaybe ("param_" <> show (ordIndex + 1)) mname
  return $ Parameter p ordIndex isAuto name

-- | Assumes that s.category == 0 (parameter)
mkHighParameter :: J.HighSymbol -> Ghidra HighParameter
mkHighParameter s = runIO $ do
  size :: Int32 <- Java.call s "getSize"
  ordIndex :: Int32 <- Java.call s "getCategoryIndex"
  mname :: Maybe Text <- traverse Java.reify . maybeNull =<< Java.call s "getName"
  let name = fromMaybe ("param_" <> show (ordIndex + 1)) mname
  pure $ HighParameter s ordIndex name size

-- | Probably not what you want, since decompilation can uncover more parameters
-- that were unknown at the low level. See 'getHighParams'
getLowParams :: J.Function -> Ghidra [Parameter]
getLowParams fn =
  runIO (Java.call fn "getParameters" >>= Java.reify) >>= traverse mkParameter

getHighParams :: J.HighFunction -> Ghidra [HighParameter]
getHighParams fn = do
  proto :: J.FunctionPrototype <-
    runIO $ Java.call fn "getFunctionPrototype" >>= JNI.newGlobalRef
  paramCount :: Int32 <- runIO $ Java.call proto "getNumParams"
  traverse (runIO . Java.call proto "getParam" >=> mkHighParameter) [0 .. paramCount - 1]

getFunctionThunkAddresses :: J.Function -> Ghidra [J.Address]
getFunctionThunkAddresses func = do
  mArr :: Maybe J.AddressArray <- maybeNull <$> runIO (Java.call func "getFunctionThunkAddresses" True)
  case mArr of
    Nothing -> return []
    Just arr -> do
      n <- runIO $ JNI.getArrayLength arr
      forM [0 .. n - 1] $ \i -> do
        obj :: J.Object <- runIO $ JNI.getObjectArrayElement arr i >>= JNI.newGlobalRef
        return $ coerce obj

getExternalLocation :: J.Function -> Ghidra (Maybe J.ExternalLocationDB)
getExternalLocation fn = runIO $ fmap coerce . maybeNull <$> (Java.call fn "getExternalLocation" :: IO J.ExternalLocation)


