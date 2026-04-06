module Flint.Shell.Types where

import Flint.Prelude

import Data.IORef

import Flint.Types.Analysis.Path.Matcher.PathPrep (PathPrep, mkPathPrep)
import Flint.Types.Analysis (TaintPropagator)
import Flint.Types.Analysis.Path.Matcher (Prim)
import Flint.Analysis.Path.Matcher (TypedStmt, asStmts)
import qualified Flint.Analysis.LibC as LibC
import qualified Flint.Analysis.Path.Matcher.Primitives.Library as PrimLib
import Flint.Types.Analysis.Path.Matcher.Primitives (KnownFunc)
import Flint.Types.Cfg.Store (CfgStore)

import Blaze.Cfg.Path (PilPath)
import Blaze.Import.Xref (Xref)
import Blaze.Types.Function (Function)
import qualified Blaze.Types.Pil as Pil

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text


type PathId = Int

-- | How to display a path.
data PathViewMode
  = ViewReduced         -- ^ Default: copy-propagated/reduced view
  | ViewRaw             -- ^ @!@ suffix: raw/unreduced PIL
  | ViewContextStripped -- ^ @!!@ suffix: target function only, with propagated caller args
  deriving (Eq, Show, Generic)

-- | A path reference with a view mode.
--   @PathRef 3 ViewReduced@ = reduced (default)
--   @PathRef 3 ViewRaw@ = raw/unreduced (3!)
--   @PathRef 3 ViewContextStripped@ = context-stripped (3!!)
data PathRef = PathRef
  { pathRefId :: PathId
  , viewMode  :: PathViewMode
  } deriving (Eq, Show, Generic)

-- | For backwards compatibility
wantRaw :: PathRef -> Bool
wantRaw (PathRef _ ViewRaw) = True
wantRaw _ = False

-- | Parse path references from command arguments. Supports:
--   Individual args:   1 2 3       (reduced by default)
--   Raw modifier:      1! 2!       (unreduced/raw view)
--   Bracket lists:     [1, 2, 3]
--   Ranges:            1..5  or  1-5  (expands to [1,2,3,4,5])
--   Mixed:             [1, 3..7, 10!]
parsePathRefs :: [Text] -> [PathRef]
parsePathRefs args =
  let joined = Text.intercalate " " args
      stripped = Text.filter (\c -> c /= '[' && c /= ']') joined
      parts = filter (not . Text.null)
            . fmap Text.strip
            . Text.split (\c -> c == ',' || isSpace c)
            $ stripped
  in concatMap parseRefPart parts

-- | Backwards-compatible: parse path IDs, ignoring any modifiers
parsePathIds :: [Text] -> [PathId]
parsePathIds = fmap pathRefId . parsePathRefs

-- | Parse a suffix like @!@ or @!!@ into a PathViewMode.
parseSuffix :: Text -> PathViewMode
parseSuffix "!!" = ViewContextStripped
parseSuffix "!"  = ViewRaw
parseSuffix _    = ViewReduced

parseRefPart :: Text -> [PathRef]
parseRefPart t
  -- Try "a..b" range first
  | (a, rest) <- Text.breakOn ".." t
  , not (Text.null rest)
  , Just lo <- readMaybe (Text.unpack . Text.strip . Text.dropWhileEnd (== '!') $ a)
  , Just hi <- readMaybe (Text.unpack . Text.strip . Text.dropWhileEnd (== '!') $ Text.drop 2 rest)
  = fmap (`PathRef` ViewReduced) [lo..hi]
  -- Try "a-b" range (only if both sides are digits, to avoid negative numbers)
  | Just (a, b) <- splitRange (Text.dropWhileEnd (== '!') t)
  , Just lo <- readMaybe (Text.unpack a)
  , Just hi <- readMaybe (Text.unpack b)
  = fmap (`PathRef` ViewReduced) [lo..hi]
  -- Single number, possibly with ! or !! suffix
  | (numPart, suffix) <- Text.span isDigit t
  , Just n <- readMaybe (Text.unpack numPart)
  = [PathRef n (parseSuffix suffix)]
  | otherwise = []
  where
    -- Split "3-10" but not "-3" (negative number)
    splitRange :: Text -> Maybe (Text, Text)
    splitRange s =
      let digits = Text.takeWhile isDigit s
      in if Text.null digits
         then Nothing
         else case Text.uncons (Text.drop (Text.length digits) s) of
           Just ('-', rest) | not (Text.null rest) -> Just (digits, rest)
           _ -> Nothing

-- | Caller context info, stored when a path was sampled with --context-depth.
data CallerContext = CallerContext
  { innerStmtAddrs  :: HashSet Address
    -- ^ Statement addresses from the target function's intraprocedural path.
    --   Used to filter the reduced stmts for the @!!@ view.
  , resolvedParams  :: [(Text, Pil.Expression)]
    -- ^ @(paramName, argExpression)@ pairs from the immediate caller.
  } deriving (Show, Generic)

-- | Resolve which statements to show for a cached path.
resolveStmts :: CachedPath -> PathViewMode -> [Pil.Stmt]
resolveStmts cp ViewRaw = cp ^. #pilPath
resolveStmts cp ViewReduced = case cp ^. #pathPrep of
  Just prep -> asStmts $ prep ^. #stmts
  Nothing   -> cp ^. #pilPath
resolveStmts cp ViewContextStripped = case cp ^. #callerContext of
  Just ctx ->
    let allStmts = resolveStmts cp ViewReduced
        targetAddrs = ctx ^. #innerStmtAddrs
    in filter (\s -> HashSet.member (s ^. #addr) targetAddrs) allStmts
  Nothing -> resolveStmts cp ViewReduced

data CachedPath = CachedPath
  { pilPath     :: [Pil.Stmt]
  , fullPath    :: PilPath
  , sourceFunc  :: Function
  , pathPrep    :: Maybe (PathPrep TypedStmt)
  , pathPrepTaintVersion :: Int
  , callerContext :: Maybe CallerContext
  } deriving (Generic)

data ShellState = ShellState
  { cfgStore    :: CfgStore
  , pathCache   :: IORef (HashMap PathId CachedPath)
  , nextPathId  :: IORef PathId
  , useSolver   :: IORef Bool
  , baseOffset  :: Address
  , pathTags    :: IORef (HashMap PathId Text, HashMap Text PathId)
  , userTaintPropagators :: IORef [TaintPropagator]
  , userPrims :: IORef [Prim]
  , userKnownFuncs :: IORef [KnownFunc]
  , taintConfigVersion :: IORef Int
  , inspectAddr :: Maybe (Address -> IO (Maybe Text))
  , decompFunc  :: Maybe (Address -> IO (Maybe Text))
  , saveToDb    :: Maybe (FilePath -> IO (Either Text FilePath))
  , xrefsTo     :: Maybe (Address -> IO [Xref])
  , lookupSymbol :: Maybe (Text -> IO (Maybe Address))
  } deriving (Generic)

data CommandResult
  = ResultText Text
  | ResultPaths [(PathId, Text)]
  | ResultTextAndPaths Text [(PathId, Text)]
    -- ^ Header text followed by path results
  | ResultSolver [(PathId, Text)]
  | ResultWMIs [(PathId, [Text])]
  | ResultFunctions [(Text, Address)] [(Text, Maybe Text)]
    -- ^ (internal: name+address, extern: name+library)
  | ResultOk Text
  | ResultError Text
  deriving (Eq, Show, Generic)

initShellState
  :: CfgStore -> Address -> Bool
  -> Maybe (Address -> IO (Maybe Text))
  -> Maybe (Address -> IO (Maybe Text))
  -> Maybe (FilePath -> IO (Either Text FilePath))
  -> Maybe (Address -> IO [Xref])
  -> Maybe (Text -> IO (Maybe Address))
  -> IO ShellState
initShellState store base solver mInspect mDecomp mSave mXrefs mLookupSym = do
  cache <- newIORef HashMap.empty
  nextId <- newIORef 0
  solverRef <- newIORef solver
  tags <- newIORef (HashMap.empty, HashMap.empty)
  userTaints <- newIORef []
  userPrimsRef <- newIORef []
  userKnownFuncsRef <- newIORef []
  taintVersionRef <- newIORef 0
  return ShellState
    { cfgStore = store
    , pathCache = cache
    , nextPathId = nextId
    , useSolver = solverRef
    , baseOffset = base
    , pathTags = tags
    , userTaintPropagators = userTaints
    , userPrims = userPrimsRef
    , userKnownFuncs = userKnownFuncsRef
    , taintConfigVersion = taintVersionRef
    , inspectAddr = mInspect
    , decompFunc = mDecomp
    , saveToDb = mSave
    , xrefsTo = mXrefs
    , lookupSymbol = mLookupSym
    }

currentTaintConfigVersion :: ShellState -> IO Int
currentTaintConfigVersion st = readIORef (st ^. #taintConfigVersion)

bumpTaintConfigVersion :: ShellState -> IO ()
bumpTaintConfigVersion st = modifyIORef' (st ^. #taintConfigVersion) (+ 1)

mkCachedPath
  :: ShellState
  -> [Pil.Stmt]
  -> PilPath
  -> Function
  -> Maybe (PathPrep TypedStmt)
  -> IO CachedPath
mkCachedPath st stmts fullPath' sourceFunc' pathPrep' = do
  taintVersion <- currentTaintConfigVersion st
  return CachedPath
    { pilPath = stmts
    , fullPath = fullPath'
    , sourceFunc = sourceFunc'
    , pathPrep = pathPrep'
    , pathPrepTaintVersion = taintVersion
    , callerContext = Nothing
    }

mkCachedPathWithContext
  :: ShellState
  -> [Pil.Stmt]
  -> PilPath
  -> Function
  -> Maybe (PathPrep TypedStmt)
  -> CallerContext
  -> IO CachedPath
mkCachedPathWithContext st stmts fullPath' sourceFunc' pathPrep' ctx = do
  cp <- mkCachedPath st stmts fullPath' sourceFunc' pathPrep'
  return $ cp { callerContext = Just ctx }

ensureCurrentPathPrep :: ShellState -> PathId -> CachedPath -> IO (PathPrep TypedStmt)
ensureCurrentPathPrep st pid cp = do
  taintVersion <- currentTaintConfigVersion st
  case cp ^. #pathPrep of
    Just prep
      | cp ^. #pathPrepTaintVersion == taintVersion -> return prep
    _ -> do
      tps <- getAllTaintPropagators st
      let prep = mkPathPrep tps (cp ^. #pilPath)
          cp' = cp
            & (#pathPrep ?~ prep)
            & #pathPrepTaintVersion .~ taintVersion
      modifyIORef' (st ^. #pathCache) (HashMap.adjust (const cp') pid)
      return prep

allocPathId :: ShellState -> IO PathId
allocPathId st = do
  pid <- readIORef (st ^. #nextPathId)
  modifyIORef' (st ^. #nextPathId) (+ 1)
  return pid

insertPath :: ShellState -> CachedPath -> IO PathId
insertPath st cp = do
  pid <- allocPathId st
  modifyIORef' (st ^. #pathCache) (HashMap.insert pid cp)
  return pid

lookupPath :: ShellState -> PathId -> IO (Maybe CachedPath)
lookupPath st pid = HashMap.lookup pid <$> readIORef (st ^. #pathCache)

deletePath :: ShellState -> PathId -> IO Bool
deletePath st pid = do
  cache <- readIORef (st ^. #pathCache)
  if HashMap.member pid cache
    then do
      modifyIORef' (st ^. #pathCache) (HashMap.delete pid)
      return True
    else return False

allPaths :: ShellState -> IO (HashMap PathId CachedPath)
allPaths st = readIORef (st ^. #pathCache)

-- | Tag a path with a name. Removes any previous tag on the same path ID,
--   and any previous path using the same tag name.
tagPath :: ShellState -> PathId -> Text -> IO ()
tagPath st pid name = modifyIORef' (st ^. #pathTags) $ \(idToName, nameToId) ->
  let -- Remove old tag for this path ID if any
      oldName = HashMap.lookup pid idToName
      nameToId' = maybe nameToId (`HashMap.delete` nameToId) oldName
      -- Remove old path ID for this tag name if any
      oldPid = HashMap.lookup name nameToId'
      idToName' = maybe idToName (`HashMap.delete` idToName) oldPid
  in (HashMap.insert pid name idToName', HashMap.insert name pid nameToId')

-- | Remove tag from a path.
untagPath :: ShellState -> PathId -> IO ()
untagPath st pid = modifyIORef' (st ^. #pathTags) $ \(idToName, nameToId) ->
  case HashMap.lookup pid idToName of
    Nothing -> (idToName, nameToId)
    Just name -> (HashMap.delete pid idToName, HashMap.delete name nameToId)

-- | Look up a tag name for a path ID.
lookupTag :: ShellState -> PathId -> IO (Maybe Text)
lookupTag st pid = do
  (idToName, _) <- readIORef (st ^. #pathTags)
  return $ HashMap.lookup pid idToName

-- | Look up a path ID by tag name.
lookupTaggedPath :: ShellState -> Text -> IO (Maybe PathId)
lookupTaggedPath st name = do
  (_, nameToId) <- readIORef (st ^. #pathTags)
  return $ HashMap.lookup name nameToId

-- | Get all tagged path IDs.
taggedPathIds :: ShellState -> IO (HashSet PathId)
taggedPathIds st = do
  (idToName, _) <- readIORef (st ^. #pathTags)
  return $ HashSet.fromList $ HashMap.keys idToName

-- | Parse path references, resolving tag names via ShellState.
--   Falls back to numeric parsing if not a known tag.
resolvePathRefs :: ShellState -> [Text] -> IO [PathRef]
resolvePathRefs st args = do
  (_, nameToId) <- readIORef (st ^. #pathTags)
  let resolve part =
        let tagName = Text.dropWhileEnd (== '!') part
            suffix = Text.drop (Text.length tagName) part
        in case HashMap.lookup tagName nameToId of
          Just pid -> [PathRef pid (parseSuffix suffix)]
          Nothing  -> parseRefPart part
      joined = Text.intercalate " " args
      stripped = Text.filter (\c -> c /= '[' && c /= ']') joined
      parts = filter (not . Text.null)
            . fmap Text.strip
            . Text.split (\c -> c == ',' || isSpace c)
            $ stripped
  return $ concatMap resolve parts

-- | Like resolvePathRefs but returns only PathIds.
resolvePathIds :: ShellState -> [Text] -> IO [PathId]
resolvePathIds st args = fmap pathRefId <$> resolvePathRefs st args

-- | Get all active taint propagators (LibC defaults + user-added).
getAllTaintPropagators :: ShellState -> IO [TaintPropagator]
getAllTaintPropagators st = do
  user <- readIORef (st ^. #userTaintPropagators)
  return $ LibC.taintPropagators <> user

-- | Get all prims (built-in + user-defined).
getAllPrims :: ShellState -> IO [Prim]
getAllPrims st = do
  user <- readIORef (st ^. #userPrims)
  return $ PrimLib.allPrims <> user
