module Flint.Shell.Commands.Paths
  ( sampleCommand
  , showCommand
  , pshowCommand
  , reduceCommand
  , freeCommand
  , pathsCommand
  ) where

import Flint.Prelude

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import qualified Flint.Cfg.Store as Store
import Flint.Cfg.Path (samplesFromQuery)
import Flint.Query (onionSampleBasedOnFuncSize)
import Flint.Types.Analysis.Path.Matcher.PathPrep (PathPrep, mkPathPrep)
import Flint.Analysis.Path.Matcher (TypedStmt, asStmts)
import Flint.Types.Query (QueryExpandAllOpts(..), QueryTargetOpts(..), Query(..))

import Blaze.Types.Function (Function)
import Blaze.Pretty (pretty', PStmts(PStmts))
import qualified Blaze.Types.Cfg.Path as Path

import Numeric (showHex)
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text


sampleCommand :: ShellCommand
sampleCommand = ShellCommand
  { cmdName = "sample"
  , cmdAliases = ["sp"]
  , cmdHelp = "Sample paths from a function"
  , cmdUsage = "sample <func> [count] [@ <addr> [addr ...]]"
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

reduceCommand :: ShellCommand
reduceCommand = ShellCommand
  { cmdName = "reduce"
  , cmdAliases = ["red"]
  , cmdHelp = "Reduce a path via copy/constant propagation"
  , cmdUsage = "reduce <path_id>"
  , cmdAction = reducePath
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

-- | Parse a hex address like "0x401000" or a decimal number
parseAddress :: Text -> Maybe Address
parseAddress t = case Text.stripPrefix "0x" t of
  Just hex -> intToAddr <$> readMaybe ("0x" <> Text.unpack hex)
  Nothing -> intToAddr <$> readMaybe (Text.unpack t)

-- | Show an address as hex
showAddr :: Address -> Text
showAddr addr = "0x" <> Text.pack (showHex (addrToInt addr) "")

-- | Find a function by name or address
findFunction :: ShellState -> Text -> IO (Maybe Function)
findFunction st nameOrAddr = do
  funcMap <- Store.getFuncNameMapping (st ^. #cfgStore)
  case HashMap.lookup nameOrAddr funcMap of
    Just f -> return (Just f)
    Nothing -> case parseAddress nameOrAddr of
      Nothing -> return Nothing
      Just addr -> do
        funcs <- Store.getInternalFuncs (st ^. #cfgStore)
        return $ find (\f -> f ^. #address == addr) funcs

samplePaths :: ShellState -> [Text] -> IO CommandResult
samplePaths _st [] = return $ ResultError "Usage: sample <func> [count] [@ <addr> [addr ...]]"
samplePaths st (funcArg : rest) = do
  mFunc <- findFunction st funcArg
  case mFunc of
    Nothing -> return $ ResultError $ "Function not found: " <> funcArg
    Just func -> do
      -- Split args on "@": before = [count], after = [addrs]
      let (beforeAt, afterAt) = break (== "@") rest
          mCount = case beforeAt of
            (n : _) -> readMaybe (Text.unpack n) :: Maybe Int
            _ -> Nothing
          targetAddrs = case afterAt of
            ("@" : addrArgs) -> mapMaybe parseAddress addrArgs
            _ -> []
      paths <- case NE.nonEmpty targetAddrs of
        Just addrs -> do
          -- Targeted sampling: paths must go through these addresses
          let count = fromMaybe 20 mCount
              targets = fmap (func,) addrs
              q = QueryTarget $ QueryTargetOpts
                { mustReachSome = targets
                , callExpandDepthLimit = 0  -- intraprocedural only
                , numSamples = fromIntegral count
                }
          catch
            (samplesFromQuery (st ^. #cfgStore) func q)
            (\(e :: SomeException) -> do
              warn $ "Target sampling error: " <> show e
              return [])
        Nothing -> case mCount of
          Just count -> do
            let q = QueryExpandAll $ QueryExpandAllOpts
                  { callExpandDepthLimit = 0
                  , numSamples = fromIntegral count
                  }
            samplesFromQuery (st ^. #cfgStore) func q
          Nothing ->
            fromMaybe [] <$> onionSampleBasedOnFuncSize 1.0 (st ^. #cfgStore) func
      case paths of
        [] -> return $ ResultOk $ "No paths sampled from " <> func ^. #name
              <> if not (null targetAddrs)
                 then " through " <> Text.intercalate ", " (fmap showAddr targetAddrs)
                 else ""
        _ -> do
          results <- forM paths $ \path -> do
            let stmts = Path.toStmts path
            pid <- insertPath st CachedPath
              { pilPath = stmts
              , sourceFunc = func
              , pathPrep = Nothing
              }
            let summary = "path " <> show pid
                  <> " (" <> show (length stmts) <> " stmts"
                  <> ", func: " <> func ^. #name <> ")"
            return (pid, summary)
          return $ ResultPaths results

showPaths :: ShellState -> [Text] -> IO CommandResult
showPaths _st [] = return $ ResultError "Usage: show <path_ids>  (e.g. 0 1 2, [0,1,2], [0..5])"
showPaths st args = do
  let pids = parsePathIds args
  when (null pids) $ return ()
  results <- forM pids $ \pid -> do
    mPath <- lookupPath st pid
    case mPath of
      Nothing -> return $ "Path " <> show pid <> ": not found"
      Just cp -> do
        let stmts = case cp ^. #pathPrep of
              Just prep -> asStmts $ prep ^. #stmts
              Nothing   -> cp ^. #pilPath
            header = "=== Path " <> show pid
              <> " (func: " <> (cp ^. #sourceFunc . #name)
              <> ", " <> show (length stmts) <> " stmts) ==="
        return $ header <> "\n" <> pretty' (PStmts stmts)
  return $ ResultText $ Text.intercalate "\n\n" results

pshowStmts :: ShellState -> [Text] -> IO CommandResult
pshowStmts _st [] = return $ ResultError "Usage: pshow <path_id> [addr ...]"
pshowStmts st (pidArg : addrArgs) =
  case readMaybe (Text.unpack pidArg) :: Maybe PathId of
    Nothing -> return $ ResultError $ "Invalid path id: " <> pidArg
    Just pid -> do
      mPath <- lookupPath st pid
      case mPath of
        Nothing -> return $ ResultError $ "Path " <> show pid <> " not found"
        Just cp -> do
          let stmts = case cp ^. #pathPrep of
                Just prep -> asStmts $ prep ^. #stmts
                Nothing   -> cp ^. #pilPath
              filterAddrs = mapMaybe parseAddress addrArgs
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
      let a = addrToInt (s ^. #addr)
          t = addrToInt target
      in a >= t && a < t + 16

reducePath :: ShellState -> [Text] -> IO CommandResult
reducePath _st [] = return $ ResultError "Usage: reduce <path_ids>  (e.g. 0, [0,1,2], [0..5])"
reducePath st args = do
  let pids = parsePathIds args
  case pids of
    [] -> return $ ResultError $ "Invalid path id: " <> Text.unwords args
    _ -> do
      results <- forM pids $ \pid -> do
        mPath <- lookupPath st pid
        case mPath of
          Nothing -> return $ "Path " <> show pid <> ": not found"
          Just cp -> do
            let prep :: PathPrep TypedStmt
                prep = mkPathPrep [] (cp ^. #pilPath)
                reducedStmts = asStmts $ prep ^. #stmts
            newPid <- insertPath st CachedPath
              { pilPath = reducedStmts
              , sourceFunc = cp ^. #sourceFunc
              , pathPrep = Just prep
              }
            return $
              "Reduced path " <> show pid <> " -> new path " <> show newPid
              <> " (" <> show (length reducedStmts) <> " stmts)"
      return $ ResultOk $ Text.intercalate "\n" results

freePaths :: ShellState -> [Text] -> IO CommandResult
freePaths _st [] = return $ ResultError "Usage: free <path_ids>  (e.g. 0 1 2, [0,1,2], [0..5])"
freePaths st args = do
  let pids = parsePathIds args
  results <- forM pids $ \pid -> do
    deleted <- deletePath st pid
    if deleted
      then return $ "Freed path " <> show pid
      else return $ "Path " <> show pid <> " not found"
  return $ ResultOk $ Text.intercalate "\n" results

listPaths :: ShellState -> [Text] -> IO CommandResult
listPaths st _args = do
  cache <- allPaths st
  case HashMap.toList cache of
    [] -> return $ ResultOk "No paths cached."
    entries -> do
      let sorted = sortOn fst entries
          rows = fmap (\(pid, cp) ->
            let nStmts = length $ cp ^. #pilPath
                funcName = cp ^. #sourceFunc . #name
                reduced = case cp ^. #pathPrep of
                  Just _ -> " [reduced]"
                  Nothing -> ""
            in (pid, "func: " <> funcName <> ", " <> show nStmts <> " stmts" <> reduced)
            ) sorted
      return $ ResultPaths rows
