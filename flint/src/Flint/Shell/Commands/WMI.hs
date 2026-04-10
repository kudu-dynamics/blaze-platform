module Flint.Shell.Commands.WMI
  ( wmisCommand
  , checkWMICommand
  ) where

import Flint.Prelude

import Data.IORef

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import Flint.Types.Analysis.Path.Matcher (Prim)
import Flint.Types.Analysis.Path.Matcher.PathPrep (mkPathPrep)
import Flint.Types.Analysis.Path.Matcher.Primitives (CallableWMI)
import Flint.Query (matchAndReturnCallablePrim, chooseSolver)
import qualified Flint.Types.CachedMap as CM
import qualified Blaze.Concurrent as Conc

import Blaze.Pretty (pretty')
import qualified Blaze.Types.Function as Func

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text


wmisCommand :: ShellCommand
wmisCommand = ShellCommand
  { cmdName = "wmis"
  , cmdAliases = ["lw"]
  , cmdHelp = "List available WMI primitives"
  , cmdUsage = "wmis"
  , cmdAction = listWMIs
  }

checkWMICommand :: ShellCommand
checkWMICommand = ShellCommand
  { cmdName = "check-wmi"
  , cmdAliases = ["cw"]
  , cmdHelp = "Check path(s) for a WMI primitive (use 'all' to check all)"
  , cmdUsage = "check-wmi <wmi_name|all> <path_id> [path_id ...]"
  , cmdAction = checkWMIs
  }

listWMIs :: ShellState -> [Text] -> IO CommandResult
listWMIs st _args = do
  prims <- getAllPrims st
  let rows = fmap formatPrim prims
  return $ ResultText $ Text.unlines rows
  where
    formatPrim :: Prim -> Text
    formatPrim p =
      let spec = p ^. #primType
          name = spec ^. #name
          vars = Text.intercalate ", "
                 . fmap (\v -> v ^. #unSymbol)
                 . HashSet.toList
                 $ spec ^. #vars
          locs = Text.intercalate ", "
                 . fmap (\l -> l ^. #unSymbol)
                 . HashSet.toList
                 $ spec ^. #locations
      in "  " <> name <> "  vars: {" <> vars <> "}  locations: {" <> locs <> "}"

findPrimByName :: [Prim] -> Text -> Maybe Prim
findPrimByName prims name =
  find (\p -> Text.toLower (p ^. #primType . #name) == Text.toLower name) (reverse prims)

checkWMIs :: ShellState -> [Text] -> IO CommandResult
checkWMIs _st [] = return $ ResultError "Usage: check-wmi <wmi_name|all> <path_ids>"
checkWMIs _st [_] = return $ ResultError "Usage: check-wmi <wmi_name|all> <path_ids>"
checkWMIs st (wmiName : pidArgs) = do
  allPrims' <- getAllPrims st
  primsToCheck <- case Text.toLower wmiName of
    "all" -> return $ Right allPrims'
    _ -> case findPrimByName allPrims' wmiName of
      Nothing -> return $ Left $
        "Unknown WMI: " <> wmiName <> ". Use 'wmis' to list available primitives, or 'all' to check all."
      Just prim -> return $ Right [prim]
  case primsToCheck of
    Left err -> return $ ResultError err
    Right prims -> do
      refs <- resolvePathRefs st pidArgs
      useSolve <- readIORef (st ^. #useSolver)
      let solver = chooseSolver useSolve
          pool = st ^. #cfgStore . #workerPool
      callablePrimSnapshot <- CM.getSnapshot $ st ^. (#cfgStore . #callablePrims)
      -- Prepare all paths (sequential — needs ShellState IO for cache)
      preparedPaths <- forM refs $ \(PathRef pid mode) -> do
        mPath <- lookupPath st pid
        case mPath of
          Nothing -> return (pid, Nothing)
          Just cp -> do
            prep <- if mode == ViewRaw
              then do
                tps <- getAllTaintPropagators st
                return $ mkPathPrep tps (cp ^. #pilPath)
              else ensureCurrentPathPrep st pid cp
            return (pid, Just (prep, cp ^. #sourceFunc))
      -- Build all (path, prim) combos and check concurrently
      let combos = [ (pid, prep', func, prim)
                   | (pid, Just (prep', func)) <- preparedPaths
                   , prim <- prims
                   ]
      matchResults <- Conc.pooledForConcurrently pool combos $
        \(pid, prep', func, prim) ->
          catch
            ((pid,) <$> matchAndReturnCallablePrim 10 solver callablePrimSnapshot func prep' prim)
            (\(e :: SomeException) -> do
              warn $ "Error checking WMI: " <> show e
              return (pid, []))
      -- Group results by path ID
      let notFound = [(pid, ["Path not found"]) | (pid, Nothing) <- preparedPaths]
          grouped :: [(Int, [CallableWMI])]
          grouped = HashMap.toList
                  . fmap concat
                  . HashMap.fromListWith (<>)
                  $ fmap (\(pid, ms) -> (pid, [ms])) matchResults
          formatted = fmap (\(pid, ms) -> case ms of
            [] -> (pid, ["No match"])
            xs -> (pid, fmap formatCallableWMI xs)) grouped
      return . ResultWMIs $ notFound <> formatted

formatCallableWMI :: CallableWMI -> Text
formatCallableWMI cwmi =
  let primName = cwmi ^. #prim . #name
      funcName = cwmi ^. #func . Func._name
      vars = fmap (\(k, v) -> "    " <> pretty' k <> " = " <> pretty' (fst v))
             . HashMap.toList
             $ cwmi ^. #varMapping
      locs = fmap (\(k, v) -> "    " <> pretty' k <> " = " <> either (view #name) show v)
             . HashMap.toList
             $ cwmi ^. #locations
  in Text.unlines $
    [ "  MATCH: " <> primName <> " in " <> funcName
    , "  Variables:" ] <> vars <>
    [ "  Locations:" ] <> locs
