module Flint.Shell.Commands.WMI
  ( wmisCommand
  , checkWMICommand
  ) where

import Flint.Prelude

import Data.IORef

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import Flint.Types.Analysis.Path.Matcher (Prim)
import Flint.Types.Analysis.Path.Matcher.PathPrep (PathPrep, mkPathPrep)
import Flint.Analysis.Path.Matcher (TypedStmt)
import Flint.Types.Analysis.Path.Matcher.Primitives (CallableWMI)
import qualified Flint.Analysis.Path.Matcher.Primitives.Library as PrimLib
import Flint.Query (matchAndReturnCallablePrim, chooseSolver)
import qualified Flint.Types.CachedMap as CM

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
listWMIs _st _args = do
  let prims = PrimLib.allPrims
      rows = fmap formatPrim prims
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

findPrimByName :: Text -> Maybe Prim
findPrimByName name =
  find (\p -> Text.toLower (p ^. #primType . #name) == Text.toLower name) PrimLib.allPrims

checkWMIs :: ShellState -> [Text] -> IO CommandResult
checkWMIs _st [] = return $ ResultError "Usage: check-wmi <wmi_name|all> <path_ids>"
checkWMIs _st [_] = return $ ResultError "Usage: check-wmi <wmi_name|all> <path_ids>"
checkWMIs st (wmiName : pidArgs) = do
  primsToCheck <- case Text.toLower wmiName of
    "all" -> return $ Right PrimLib.allPrims
    _ -> case findPrimByName wmiName of
      Nothing -> return $ Left $
        "Unknown WMI: " <> wmiName <> ". Use 'wmis' to list available primitives, or 'all' to check all."
      Just prim -> return $ Right [prim]
  case primsToCheck of
    Left err -> return $ ResultError err
    Right prims -> do
      refs <- resolvePathRefs st pidArgs
      useSolve <- readIORef (st ^. #useSolver)
      let solver = chooseSolver useSolve
      callablePrimSnapshot <- CM.getSnapshot $ st ^. (#cfgStore . #callablePrims)
      results <- forM refs $ \(PathRef pid raw) -> do
        mPath <- lookupPath st pid
        case mPath of
          Nothing -> return (pid, ["Path not found"])
          Just cp -> do
            let prep :: PathPrep TypedStmt
                prep = if raw
                  then mkPathPrep [] (cp ^. #pilPath)
                  else case cp ^. #pathPrep of
                    Just existing -> existing
                    Nothing -> mkPathPrep [] (cp ^. #pilPath)
                func = cp ^. #sourceFunc
            allMatches <- fmap concat . forM prims $ \prim ->
              catch
                (matchAndReturnCallablePrim 10 solver callablePrimSnapshot func prep prim)
                (\(e :: SomeException) -> do
                  warn $ "Error checking WMI: " <> show e
                  return [])
            case allMatches of
              [] -> return (pid, ["No match"])
              xs -> return (pid, fmap formatCallableWMI xs)
      return $ ResultWMIs results

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
