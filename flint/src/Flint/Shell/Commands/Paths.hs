module Flint.Shell.Commands.Paths
  ( sampleCommand
  , showCommand
  , pshowCommand
  , freeCommand
  , pathsCommand
  , tagCommand
  , freeUntaggedCommand
  , expandCommand
  , findFunction
  , expandPathToDepth
  , parseDepthArg
  , parseContextDepthArg
  , parseAddress
  ) where

import Flint.Prelude

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import qualified Flint.Cfg.Store as Store
import Flint.Cfg.Path (samplesFromQuery, sampleWithCallerContext, timingLog)
import Blaze.Cfg.Path (expandCallWithNewInnerPathIds)
import Flint.Query (onionSampleBasedOnFuncSize)
import Flint.Types.Analysis.Path.Matcher.PathPrep (mkPathPrep)
import Flint.Analysis.Path.Matcher (asStmts)
import Flint.Types.Query (QueryExpandAllOpts(..), QueryTargetOpts(..), Query(..))
import Flint.Util (timeIt)

import Blaze.Cfg.Interprocedural (getCallTargetFunction)
import Blaze.Types.Cfg (CfNode(Call), CallNode)
import Blaze.Types.Function (Function, FuncParamInfo(..))
import Blaze.Pretty (pretty', PStmts(PStmts))
import Blaze.Types.Cfg.Path (PilPath)
import qualified Blaze.Types.Cfg.Path as Path
import qualified Blaze.Types.Path as P
import qualified Blaze.Types.Pil as Pil
import Flint.Types.Cfg.Store (CfgStore)

import Numeric (showHex)
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text


sampleCommand :: ShellCommand
sampleCommand = ShellCommand
  { cmdName = "sample"
  , cmdAliases = ["sp"]
  , cmdHelp = "Sample paths from a function"
  , cmdUsage = "sample [count] <func> [@ <addr> [addr ...]] [--depth N] [--context-depth N] [--unrollLoops]"
  , cmdAction = samplePaths
  }

showCommand :: ShellCommand
showCommand = ShellCommand
  { cmdName = "show"
  , cmdAliases = ["sh"]
  , cmdHelp = "Show path statements"
  , cmdUsage = "show <path_id> [path_id ...]"
  , cmdAction = showPaths
  }

freeCommand :: ShellCommand
freeCommand = ShellCommand
  { cmdName = "free"
  , cmdAliases = ["fr"]
  , cmdHelp = "Free cached paths"
  , cmdUsage = "free <path_id> [path_id ...]"
  , cmdAction = freePaths
  }

pshowCommand :: ShellCommand
pshowCommand = ShellCommand
  { cmdName = "pshow"
  , cmdAliases = ["ps"]
  , cmdHelp = "Show raw PIL Haskell types for statements"
  , cmdUsage = "pshow <path_id> [addr ...]"
  , cmdAction = pshowStmts
  }

pathsCommand :: ShellCommand
pathsCommand = ShellCommand
  { cmdName = "paths"
  , cmdAliases = ["lp"]
  , cmdHelp = "List all cached paths"
  , cmdUsage = "paths"
  , cmdAction = listPaths
  }

tagCommand :: ShellCommand
tagCommand = ShellCommand
  { cmdName = "tag"
  , cmdAliases = ["tg"]
  , cmdHelp = "Tag a path with a name"
  , cmdUsage = "tag <path_id> <name>"
  , cmdAction = tagPathCmd
  }

freeUntaggedCommand :: ShellCommand
freeUntaggedCommand = ShellCommand
  { cmdName = "free-untagged"
  , cmdAliases = ["fu"]
  , cmdHelp = "Free all untagged paths"
  , cmdUsage = "free-untagged"
  , cmdAction = freeUntaggedPaths
  }

getParamName :: FuncParamInfo -> Text
getParamName (FuncParamInfo p) = p ^. #name
getParamName (FuncVarArgInfo p) = p ^. #name

-- | Parse a hex address like "0x401000" or a decimal number,
-- using the given address space (from the binary) for correct pointer size.
parseAddressWithSpace :: AddressSpace -> Text -> Maybe Address
parseAddressWithSpace sp t = setSpace <$> parseAddress t
  where
    setSpace addr = addr & #space .~ sp

-- | Parse a hex address using the default (64-bit) address space.
parseAddress :: Text -> Maybe Address
parseAddress t = intToAddr <$> case Text.stripPrefix "0x" t of
  Just hex -> readMaybe ("0x" <> Text.unpack hex)
  Nothing -> readMaybe (Text.unpack t)

-- | Show an address as hex
showAddr :: Address -> Text
showAddr addr = "0x" <> Text.pack (showHex (addrToInt addr) "")

-- | Find a function by name or address
findFunction :: ShellState -> Text -> IO (Maybe Function)
findFunction st nameOrAddr = do
  let store = st ^. #cfgStore
  funcMap <- Store.getFuncNameMapping store
  mRef <- case HashMap.lookup nameOrAddr funcMap of
    Just fm -> return (Just fm)
    Nothing -> do
      let addrSpace = store ^. #baseOffset . #space
      case parseAddressWithSpace addrSpace nameOrAddr of
        Nothing -> return Nothing
        Just addr -> do
          refs <- Store.getInternalFuncs store
          return $ find (\fm -> fm ^. #address == addr) refs
  case mRef of
    Nothing -> return Nothing
    Just ref' -> Store.resolveFunction store ref'

-- | Parse --depth N from argument list, returning (depth, remaining args)
parseDepthArg :: [Text] -> (Int, [Text])
parseDepthArg [] = (0, [])
parseDepthArg ("--depth" : n : rest)
  | Just d <- readMaybe (Text.unpack n) = (d, rest)
parseDepthArg (x : xs) =
  let (d, rest) = parseDepthArg xs
  in (d, x : rest)

-- | Parse --context-depth N from argument list, returning (depth, remaining args)
parseContextDepthArg :: [Text] -> (Int, [Text])
parseContextDepthArg [] = (0, [])
parseContextDepthArg ("--context-depth" : n : rest)
  | Just d <- readMaybe (Text.unpack n) = (d, rest)
parseContextDepthArg (x : xs) =
  let (d, rest) = parseContextDepthArg xs
  in (d, x : rest)

-- | Parse --unrollLoops flag from argument list, returning (flag, remaining args)
parseUnrollLoopsArg :: [Text] -> (Bool, [Text])
parseUnrollLoopsArg = go False
  where
    go found [] = (found, [])
    go _ ("--unrollLoops" : rest) = go True rest
    go found (x : xs) =
      let (f, rest) = go found xs
      in (f, x : rest)

samplePaths :: ShellState -> [Text] -> IO CommandResult
samplePaths _st [] = return $ ResultError "Usage: sample [count] <func> [@ <addr> [addr ...]] [--depth N] [--context-depth N] [--unrollLoops]"
samplePaths st allArgs' = do
  let (depth, allArgs0) = parseDepthArg allArgs'
      (contextDepth, allArgs1) = parseContextDepthArg allArgs0
      (useUnrollLoops, allArgs) = parseUnrollLoopsArg allArgs1
  -- Parse optional leading count: if first arg is a number, it's the count
  let (mLeadingCount, afterCount) = case allArgs of
        (n : rest') | Just c <- (readMaybe (Text.unpack n) :: Maybe Int) -> (Just c, rest')
        _ -> (Nothing, allArgs)
  case afterCount of
    [] -> return $ ResultError "Usage: sample [count] <func> [@ <addr> [addr ...]]"
    (funcArg : rest) -> do
      mFunc <- findFunction st funcArg
      case mFunc of
        Nothing -> return $ ResultError $ "Function not found: " <> funcArg
        Just func -> do
          let -- Split remaining args on "@": after = [addrs]
              (_, afterAt) = break (== "@") rest
              mCount = mLeadingCount
              addrSpace = func ^. #address . #space
              targetAddrs = case afterAt of
                ("@" : addrArgs) -> mapMaybe (parseAddressWithSpace addrSpace) addrArgs
                _ -> []
          -- If --context-depth is specified, use caller context sampling
          if contextDepth > 0
            then samplePathsWithContext st func (fromMaybe 10 mCount)
                   contextDepth depth targetAddrs useUnrollLoops
            else do
              (paths, samplingTime) <- timeIt $ case NE.nonEmpty targetAddrs of
                Just addrs -> do
                  -- Targeted sampling: paths must go through these addresses
                  let count = fromMaybe 20 mCount
                      targets = fmap (func,) addrs
                      q = QueryTarget $ QueryTargetOpts
                        { mustReachSome = targets
                        , callExpandDepthLimit = 0  -- intraprocedural only
                        , numSamples = fromIntegral count
                        , unrollLoops = useUnrollLoops
                        }
                  catch
                    (samplesFromQuery (st ^. #cfgStore) func q)
                    (\(e :: SomeException) -> do
                      warn $ "Target sampling error: " <> show e
                      return [])
                Nothing -> case (mCount, useUnrollLoops) of
                  (Just count, _) -> do
                    let q = QueryExpandAll $ QueryExpandAllOpts
                          { callExpandDepthLimit = 0
                          , numSamples = fromIntegral count
                          , unrollLoops = useUnrollLoops
                          }
                    samplesFromQuery (st ^. #cfgStore) func q
                  (Nothing, True) -> do
                    -- --unrollLoops without a count: use default count so the flag takes effect
                    let q = QueryExpandAll $ QueryExpandAllOpts
                          { callExpandDepthLimit = 0
                          , numSamples = 20
                          , unrollLoops = True
                          }
                    samplesFromQuery (st ^. #cfgStore) func q
                  (Nothing, False) ->
                    fromMaybe [] <$> onionSampleBasedOnFuncSize 1.0 (st ^. #cfgStore) func
              timingLog $ "[timing] samplesFromQuery (" <> func ^. #name <> "): "
                <> show (length paths) <> " paths in " <> show samplingTime

              -- Auto-expand internal calls if --depth was specified
              expandedPaths <- if depth <= 0
                then return paths
                else fmap concat . forM paths $ \path ->
                  expandPathToDepth (st ^. #cfgStore) path depth

              case expandedPaths of
                [] -> return $ ResultOk $ "No paths sampled from " <> func ^. #name
                      <> if not (null targetAddrs)
                         then " through " <> Text.intercalate ", " (fmap showAddr targetAddrs)
                         else ""
                _ -> cacheAndReturnPaths st func expandedPaths Nothing

-- | Sample paths with caller context and cache them.
samplePathsWithContext
  :: ShellState -> Function -> Int -> Int -> Int
  -> [Address]  -- ^ Target addresses (from @ args)
  -> Bool       -- ^ Use loop unrolling
  -> IO CommandResult
samplePathsWithContext st func count contextDepth calleeDepth targetAddrs useUnrollLoops = do
  (results, samplingTime) <- timeIt $
    sampleWithCallerContext (st ^. #cfgStore) func (fromIntegral count)
      contextDepth targetAddrs useUnrollLoops
  timingLog $ "[timing] sampleWithCallerContext (" <> func ^. #name <> "): "
    <> show (length results) <> " paths in " <> show samplingTime
  case results of
    [] -> return $ ResultOk $ "No paths sampled from " <> func ^. #name
          <> " with context-depth " <> show contextDepth
    _ -> do
      -- Optionally expand callee calls within the full paths
      expandedResults <- if calleeDepth <= 0
        then return results
        else fmap concat . forM results $ \(path, addrs, bindings) -> do
          expanded <- expandPathToDepth (st ^. #cfgStore) path calleeDepth
          return $ fmap (, addrs, bindings) expanded
      let mContexts = fmap (\(_, addrs, bindings) ->
            CallerContext { innerStmtAddrs = addrs, resolvedParams = bindings })
            expandedResults
          paths = fmap (\(p, _, _) -> p) expandedResults
      cacheAndReturnPaths st func paths (Just mContexts)

-- | Cache a list of paths and return ResultPaths.
cacheAndReturnPaths
  :: ShellState -> Function -> [PilPath]
  -> Maybe [CallerContext]  -- ^ If present, one per path
  -> IO CommandResult
cacheAndReturnPaths _st func [] _ =
  return $ ResultOk $ "No paths sampled from " <> func ^. #name
cacheAndReturnPaths st func paths mContexts = do
  results <- forM (zip paths [0..]) $ \(path, i :: Int) -> do
    (stmts, toStmtsTime) <- timeIt $ do
      let s = Path.toStmts path
      _ <- evaluate (length s)
      return s
    (prep, expandTime) <- timeIt $ do
      tps <- getAllTaintPropagators st
      let p = mkPathPrep tps stmts
      _ <- evaluate (length $ p ^. #stmts)
      return p
    let reducedStmts = asStmts $ prep ^. #stmts
    cp <- case mContexts >>= (!!? i) of
      Just ctx -> mkCachedPathWithContext st stmts path func (Just prep) ctx
      Nothing  -> mkCachedPath st stmts path func (Just prep)
    pid <- insertPath st cp
    timingLog $ "[timing] path " <> show pid <> ": toStmts=" <> show toStmtsTime
      <> " (" <> show (length stmts) <> " raw stmts)"
      <> ", aggressiveExpand=" <> show expandTime
      <> " (" <> show (length reducedStmts) <> " reduced stmts)"
    let summary = "path " <> show pid
          <> " (" <> show (length reducedStmts) <> " stmts"
          <> ", func: " <> func ^. #name <> ")"
    return (pid, summary)
  return $ ResultPaths results

-- | Safe list indexing
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
  | i < 0 = Nothing
  | i >= length xs = Nothing
  | otherwise = Just (xs !! i)

showPaths :: ShellState -> [Text] -> IO CommandResult
showPaths _st [] = return $ ResultError "Usage: show <path_ids>  (e.g. 0 1 2, [0,1,2], [0..5], 0! for raw, 0!! for context-stripped)"
showPaths st args = do
  refs <- resolvePathRefs st args
  results <- forM refs $ \(PathRef pid mode) -> do
    mPath <- lookupPath st pid
    mTag <- lookupTag st pid
    case mPath of
      Nothing -> return $ "Path " <> show pid <> ": not found"
      Just cp -> do
        let stmts = resolveStmts cp mode
            modeTag = case mode of
              ViewRaw -> " [raw]"
              ViewContextStripped -> " [context-stripped]"
              ViewReduced -> ""
            tagLabel = maybe "" (\t -> " \"" <> t <> "\"") mTag
            funcName = cp ^. #sourceFunc . #name
            funcSig = case (mode, cp ^. #callerContext) of
              (ViewContextStripped, Just ctx)
                | not (null $ ctx ^. #resolvedParams) ->
                  let paramBindings = fmap (\(n, e) -> n <> "=" <> pretty' e)
                                    $ ctx ^. #resolvedParams
                  in funcName <> "(" <> Text.intercalate ", " paramBindings <> ")"
              _ ->
                let paramNames = fmap getParamName $ cp ^. #sourceFunc . #params
                in funcName <> "(" <> Text.intercalate ", " paramNames <> ")"
            header = "=== Path " <> show pid <> tagLabel <> modeTag
              <> " (func: " <> funcSig
              <> ", " <> show (length stmts) <> " stmts) ==="
        return $ header <> "\n" <> pretty' (PStmts stmts)
  return $ ResultText $ Text.intercalate "\n\n" results

pshowStmts :: ShellState -> [Text] -> IO CommandResult
pshowStmts _st [] = return $ ResultError "Usage: pshow <path_id> [addr ...] (use N! for raw, N!! for context-stripped)"
pshowStmts st (pidArg : addrArgs) = do
  refs <- resolvePathRefs st [pidArg]
  case refs of
    [] -> return $ ResultError $ "Invalid path id: " <> pidArg
    (PathRef pid mode : _) -> do
      mPath <- lookupPath st pid
      case mPath of
        Nothing -> return $ ResultError $ "Path " <> show pid <> " not found"
        Just cp -> do
          let stmts = resolveStmts cp mode
              addrSpace = cp ^. #sourceFunc . #address . #space
              filterAddrs = mapMaybe (parseAddressWithSpace addrSpace) addrArgs
              filtered
                | null filterAddrs = stmts
                | otherwise = filter (\s -> any (stmtNearAddr s) filterAddrs) stmts
              rendered = fmap (\s ->
                let hdr = "--- " <> showAddr (s ^. #addr) <> " ---"
                in hdr <> "\n" <> cs (pshow s)
                ) filtered
          if null filtered
            then return $ ResultOk $ "No statements found"
              <> if not (null filterAddrs)
                 then " at " <> Text.intercalate ", " (fmap showAddr filterAddrs)
                 else ""
            else return $ ResultText $ Text.intercalate "\n\n" rendered
  where
    -- Match stmt if its address is within a small range of the target
    -- (handles Ghidra address misalignment within a basic block)
    stmtNearAddr s target =
      let a = addrToInt $ s ^. #addr
          t = addrToInt target
      in a >= t && a < t + 16

freePaths :: ShellState -> [Text] -> IO CommandResult
freePaths _st [] = return $ ResultError "Usage: free <path_ids>  (e.g. 0 1 2, [0,1,2], [0..5], or tag names)"
freePaths st args = do
  pids <- resolvePathIds st args
  results <- forM pids $ \pid -> do
    deleted <- deletePath st pid
    if deleted
      then do
        untagPath st pid
        return $ "Freed path " <> show pid
      else return $ "Path " <> show pid <> " not found"
  return $ ResultOk $ Text.intercalate "\n" results

listPaths :: ShellState -> [Text] -> IO CommandResult
listPaths st _args = do
  cache <- allPaths st
  case HashMap.toList cache of
    [] -> return $ ResultOk "No paths cached."
    entries -> do
      let sorted = sortOn fst entries
      rows <- forM sorted $ \(pid, cp) -> do
        let stmts = resolveStmts cp ViewReduced
            funcName = cp ^. #sourceFunc . #name
        mTag <- lookupTag st pid
        let tagLabel = maybe "" (\t -> " [" <> t <> "]") mTag
        return (pid, "func: " <> funcName <> ", " <> show (length stmts) <> " stmts" <> tagLabel)
      return $ ResultPaths rows

tagPathCmd :: ShellState -> [Text] -> IO CommandResult
tagPathCmd _st [] = return $ ResultError "Usage: tag <path_id> <name>"
tagPathCmd _st [_] = return $ ResultError "Usage: tag <path_id> <name>"
tagPathCmd st (pidArg : nameArg : _) = do
  pids <- resolvePathIds st [pidArg]
  case pids of
    [] -> return $ ResultError $ "Invalid path id: " <> pidArg
    (pid : _) -> do
      mPath <- lookupPath st pid
      case mPath of
        Nothing -> return $ ResultError $ "Path " <> show pid <> " not found"
        Just _ -> do
          tagPath st pid nameArg
          return $ ResultOk $ "Tagged path " <> show pid <> " as \"" <> nameArg <> "\""

freeUntaggedPaths :: ShellState -> [Text] -> IO CommandResult
freeUntaggedPaths st _args = do
  cache <- allPaths st
  tagged <- taggedPathIds st
  let untaggedPids = filter (\pid -> not $ HashSet.member pid tagged) $ HashMap.keys cache
  forM_ untaggedPids $ \pid -> deletePath st pid
  return $ ResultOk $ "Freed " <> show (length untaggedPids) <> " untagged path(s)"

expandCommand :: ShellCommand
expandCommand = ShellCommand
  { cmdName = "expand"
  , cmdAliases = ["ex"]
  , cmdHelp = "Expand a callsite in a path with callee paths"
  , cmdUsage = "expand <path_id> <callsite_addr> [count | --paths <path_ids>]"
  , cmdAction = expandCallSite
  }

expandCallSite :: ShellState -> [Text] -> IO CommandResult
expandCallSite _st [] = return $ ResultError
  "Usage: expand <path_id> <callsite_addr> [count | --paths <path_ids>]"
expandCallSite _st [_] = return $ ResultError
  "Usage: expand <path_id> <callsite_addr> [count | --paths <path_ids>]"
expandCallSite st (pidArg : addrArg : rest) = do
  -- Parse outer path ID
  pids <- resolvePathIds st [pidArg]
  case pids of
    [] -> return $ ResultError $ "Invalid path id: " <> pidArg
    (outerPid : _) -> do
      mOuterCp <- lookupPath st outerPid
      case mOuterCp of
        Nothing -> return $ ResultError $ "Path " <> show outerPid <> " not found"
        Just outerCp -> do
          -- Parse callsite address
          let addrSpace = outerCp ^. #sourceFunc . #address . #space
          case parseAddressWithSpace addrSpace addrArg of
            Nothing -> return $ ResultError $ "Invalid address: " <> addrArg
            Just callAddr -> expandWithAddr st outerPid outerCp callAddr rest

expandWithAddr :: ShellState -> PathId -> CachedPath -> Address -> [Text] -> IO CommandResult
expandWithAddr st outerPid outerCp callAddr rest = do
  let outerPilPath = outerCp ^. #fullPath
      -- Find all Call nodes in the path
      allNodes = HashSet.toList $ P.nodes outerPilPath
      callNodes = [ cn | Call cn <- allNodes ]
      -- Find the call node at the target address
      matchingCalls = filter (\cn -> cn ^. #start == callAddr) callNodes
  case matchingCalls of
    [] -> do
      -- List available call addresses for the user
      let callAddrs = fmap (\cn -> showAddr (cn ^. #start)) callNodes
      if null callAddrs
        then return $ ResultError $ "Path " <> show outerPid <> " has no call nodes"
        else return $ ResultError $ "No call at " <> showAddr callAddr
              <> ". Available calls: " <> Text.intercalate ", " callAddrs
    (callNode : _) -> do
      -- Check if call destination is a known function
      case getCallTargetFunction (callNode ^. #callDest) of
        Nothing -> return $ ResultError
          $ "Call at " <> showAddr callAddr
          <> " is an indirect call (not yet supported)"
        Just calleeFunc -> do
          -- Parse mode: --paths or count
          let (_beforePaths, pathsArgs) = break (== "--paths") rest
          case pathsArgs of
            ("--paths" : innerPathArgs) ->
              expandWithSpecificPaths st outerPid outerCp callNode calleeFunc innerPathArgs
            _ -> do
              let count = case rest of
                    (n : _) -> fromMaybe 1 (readMaybe (Text.unpack n) :: Maybe Int)
                    _ -> 1
              expandWithSampling st outerPid outerCp callNode calleeFunc count

expandWithSampling
  :: ShellState -> PathId -> CachedPath -> CallNode [Pil.Stmt]
  -> Function -> Int -> IO CommandResult
expandWithSampling st outerPid outerCp callNode calleeFunc count = do
  let q = QueryExpandAll $ QueryExpandAllOpts
        { callExpandDepthLimit = 0
        , numSamples = fromIntegral count
        , unrollLoops = False
        }
  innerPaths <- catch
    (samplesFromQuery (st ^. #cfgStore) calleeFunc q)
    (\(e :: SomeException) -> do
      warn $ "Sampling error for " <> calleeFunc ^. #name <> ": " <> show e
      return [])
  case innerPaths of
    [] -> return $ ResultError $ "No paths sampled from " <> calleeFunc ^. #name
    _ -> doExpansions st outerPid outerCp callNode innerPaths

expandWithSpecificPaths
  :: ShellState -> PathId -> CachedPath -> CallNode [Pil.Stmt]
  -> Function -> [Text] -> IO CommandResult
expandWithSpecificPaths st outerPid outerCp callNode calleeFunc innerPathArgs = do
  innerPids <- resolvePathIds st innerPathArgs
  if null innerPids
    then return $ ResultError "No valid inner path IDs provided"
    else do
      results <- forM innerPids $ \iPid -> do
        mCp <- lookupPath st iPid
        case mCp of
          Nothing -> return $ Left $ "Path " <> show iPid <> " not found"
          Just cp
            | cp ^. #sourceFunc . #address /= calleeFunc ^. #address ->
                return $ Left $ "Path " <> show iPid <> " is from "
                  <> cp ^. #sourceFunc . #name
                  <> ", but callsite targets " <> calleeFunc ^. #name
            | otherwise -> return $ Right (cp ^. #fullPath)
      let (errs, innerPilPaths) = partitionEithers results
      if null innerPilPaths
        then return $ ResultError $ Text.intercalate "\n" errs
        else do
          r <- doExpansions st outerPid outerCp callNode innerPilPaths
          if null errs
            then return r
            else case r of
              ResultPaths ps -> return $ ResultPaths ps  -- warnings are secondary
              other -> return other

doExpansions
  :: ShellState -> PathId -> CachedPath -> CallNode [Pil.Stmt]
  -> [Path.PilPath] -> IO CommandResult
doExpansions st outerPid outerCp callNode innerPilPaths = do
  let outerPilPath = outerCp ^. #fullPath
      outerFunc = outerCp ^. #sourceFunc
  results <- forM innerPilPaths $ \innerPilPath -> do
    uuid <- randomIO
    mExpanded <- expandCallWithNewInnerPathIds uuid outerPilPath callNode innerPilPath
    case mExpanded of
      Nothing -> return $ Left "Expansion failed (call node not found in path)"
      Just expandedPath -> do
        tps <- getAllTaintPropagators st
        let stmts = Path.toStmts expandedPath
            prep = mkPathPrep tps stmts
            reducedStmts = asStmts $ prep ^. #stmts
        cp <- mkCachedPath st stmts expandedPath outerFunc (Just prep)
        pid <- insertPath st cp
        let summary = "path " <> show pid
              <> " (" <> show (length reducedStmts) <> " stmts"
              <> ", expanded from path " <> show outerPid <> ")"
        return $ Right (pid, summary)
  let (errs, expanded) = partitionEithers results
  case expanded of
    [] -> return $ ResultError $ "All expansions failed: "
            <> Text.intercalate ", " errs
    _ -> return $ ResultPaths expanded

-- | Recursively expand all internal call nodes in a path up to the given depth.
expandPathToDepth
  :: CfgStore -> PilPath -> Int -> IO [PilPath]
expandPathToDepth _store path 0 = return [path]
expandPathToDepth store path dpth = do
  let allNodes = HashSet.toList $ P.nodes path
      callNodes = [ cn | Call cn <- allNodes ]
      expandableNodes = filter (isExpandable . (^. #callDest)) callNodes

  case expandableNodes of
    [] -> return [path]
    _ -> do
      results <- foldM expandOneCall [path] expandableNodes
      fmap concat . forM results $ \p ->
        expandPathToDepth store p (dpth - 1)

  where
    isExpandable callDest = case getCallTargetFunction callDest of
      Just _func -> True
      Nothing    -> False

    expandOneCall :: [PilPath] -> CallNode [Pil.Stmt] -> IO [PilPath]
    expandOneCall currentPaths callNode = do
      case getCallTargetFunction (callNode ^. #callDest) of
        Nothing -> return currentPaths
        Just calleeFunc -> do
          let q = QueryExpandAll $ QueryExpandAllOpts
                { callExpandDepthLimit = 0
                , numSamples = 1
                , unrollLoops = False
                }
          innerPaths <- catch
            (samplesFromQuery store calleeFunc q)
            (\(e :: SomeException) -> do
              warn $ "Expand error for " <> calleeFunc ^. #name <> ": " <> show e
              return [])
          case innerPaths of
            [] -> return currentPaths
            (innerPath : _) -> do
              fmap concat . forM currentPaths $ \outerPath -> do
                uuid <- randomIO
                mExpanded <- expandCallWithNewInnerPathIds uuid outerPath callNode innerPath
                case mExpanded of
                  Nothing -> return [outerPath]
                  Just expanded -> return [expanded]
