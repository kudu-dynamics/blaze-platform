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
import Ghidra.Types.Function (Function(Function), HighParameter(HighParameter), Parameter(Parameter))
import Ghidra.Types.Internal (Ghidra, runIO)
import Ghidra.Types.Pcode (SimplificationStyle(Decompile))
import qualified Ghidra.Address as Addr
import qualified Foreign.JNI as JNI
import qualified Foreign.JNI.String as JString
import Ghidra.Clang (buildClangAST, ClangAST, ClangNode)
import qualified Data.Text as Text


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

getClangAST :: GhidraState -> J.Function -> Ghidra (ClangAST ClangNode)
getClangAST gs fn = do
  flatDecAPI <- State.getFlatDecompilerAPI gs
  _ :: () <- runIO $ Java.call flatDecAPI "initialize"
  ifc :: J.DecompInterface <- runIO $ Java.call flatDecAPI "getDecompiler"
  mon <- State.getTaskMonitor gs
  res :: J.DecompilerResults <- runIO $ Java.call ifc "decompileFunction" fn (0 :: Int32) mon >>= JNI.newGlobalRef
  tokenGroup :: J.ClangTokenGroup <- runIO $ Java.call res "getCCodeMarkup"
  let rootNode :: J.ClangNode = coerce tokenGroup
  buildClangAST rootNode

simplStyleStr :: SimplificationStyle -> Text
simplStyleStr = Text.toLower . Text.pack . show

-- | This is expensive and should be performed only once per function.
decompileFunction_ :: GhidraState -> J.Function -> SimplificationStyle -> Ghidra J.DecompilerResults
decompileFunction_ gs fn st = do
  flatDecAPI <- State.getFlatDecompilerAPI gs
  _ :: () <- runIO $ Java.call flatDecAPI "initialize"
  ifc :: J.DecompInterface <- runIO $ Java.call flatDecAPI "getDecompiler"
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
  ordIndex :: Int32 <- Java.call s "getCategoryIndex"
  mname :: Maybe Text <- traverse Java.reify . maybeNull =<< Java.call s "getName"
  let name = fromMaybe ("param_" <> show (ordIndex + 1)) mname
  pure $ HighParameter s ordIndex name

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

getExternalLocation :: J.Function -> Ghidra J.ExternalLocationDB
getExternalLocation fn = runIO $ coerce (Java.call fn "getExternalLocation" :: IO J.ExternalLocation)


