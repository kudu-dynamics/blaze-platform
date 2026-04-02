module Flint.Shell.Commands.Taint
  ( taintAddCommand
  , taintListCommand
  , taintRemoveCommand
  , taintResetCommand
  ) where

import Flint.Prelude

import Data.IORef

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import Flint.Types.Analysis (Parameter(..), TaintPropagator(..))
import qualified Flint.Analysis.LibC as LibC

import qualified Data.Text as Text

-- | Parse a parameter spec: arg:0, arg:1, ret, other:nvram
-- Also accepts src:N/dst:N prefixes and bare integers
parseParam :: Text -> Maybe Parameter
parseParam t = parseParam' (stripParamPrefix t)
  where
    -- Strip optional src:/dst: prefix
    stripParamPrefix s
      | Just rest <- Text.stripPrefix "src:" s = rest
      | Just rest <- Text.stripPrefix "dst:" s = rest
      | otherwise = s
    parseParam' "ret" = Just ReturnParameter
    parseParam' "return" = Just ReturnParameter
    parseParam' s
      | Just n <- Text.stripPrefix "arg:" s = Parameter <$> readMaybe (Text.unpack n)
      | Just n <- readMaybe (Text.unpack s) = Just (Parameter n)
      | Just rest <- Text.stripPrefix "other:" s
      , not (Text.null rest) = Just (Other rest)
      | otherwise = Nothing

showParam :: Parameter -> Text
showParam ReturnParameter = "ret"
showParam (Parameter n) = "arg:" <> show n
showParam (Other t) = "other:" <> t

-- ---------------------------------------------------------------------------
-- taint-add
-- ---------------------------------------------------------------------------

taintAddCommand :: ShellCommand
taintAddCommand = ShellCommand
  { cmdName = "taint-add"
  , cmdAliases = ["ta"]
  , cmdHelp = "Add a taint propagator for a function"
  , cmdUsage = "taint-add <func> <from> <to>  (e.g. taint-add strcpy src:1 dst:0)"
  , cmdAction = taintAddAction
  }

taintAddAction :: ShellState -> [Text] -> IO CommandResult
taintAddAction _st [] = return $ ResultError
  "Usage: taint-add <func> <from> <to>\n  e.g. taint-add strcpy src:1 dst:0\n       taint-add getenv other:env ret"
taintAddAction _st [_] = return $ ResultError "Missing <from> and <to> parameters."
taintAddAction _st [_, _] = return $ ResultError "Missing <to> parameter."
taintAddAction st (func : fromArg : toArg : _) = do
  case (parseParam fromArg, parseParam toArg) of
    (Nothing, _) -> return $ ResultError $ "Invalid from parameter: " <> fromArg
      <> "\n  Use arg:N, ret, or other:<name>"
    (_, Nothing) -> return $ ResultError $ "Invalid to parameter: " <> toArg
      <> "\n  Use arg:N, ret, or other:<name>"
    (Just fromParam, Just toParam) -> do
      let prop = FunctionCallPropagator func fromParam toParam
      modifyIORef' (st ^. #userTaintPropagators) (<> [prop])
      bumpTaintConfigVersion st
      return $ ResultOk $ "Added taint propagator: " <> func
        <> " " <> showParam fromParam <> " -> " <> showParam toParam

-- ---------------------------------------------------------------------------
-- taint-list
-- ---------------------------------------------------------------------------

taintListCommand :: ShellCommand
taintListCommand = ShellCommand
  { cmdName = "taint-list"
  , cmdAliases = ["tl"]
  , cmdHelp = "List all active taint propagators"
  , cmdUsage = "taint-list"
  , cmdAction = taintListAction
  }

taintListAction :: ShellState -> [Text] -> IO CommandResult
taintListAction st _args = do
  user <- readIORef (st ^. #userTaintPropagators)
  let defaults = LibC.taintPropagators
      defaultLines = fmap (\p -> formatProp p <> "  (default)") defaults
      userLines = fmap formatProp user
      allLines = defaultLines <> userLines
  case allLines of
    [] -> return $ ResultOk "No taint propagators configured."
    _ -> return $ ResultText $ Text.unlines allLines

formatProp :: TaintPropagator -> Text
formatProp (FunctionCallPropagator func fromParam toParam) =
  "  " <> func <> " : " <> showParam fromParam <> " -> " <> showParam toParam

-- ---------------------------------------------------------------------------
-- taint-remove
-- ---------------------------------------------------------------------------

taintRemoveCommand :: ShellCommand
taintRemoveCommand = ShellCommand
  { cmdName = "taint-remove"
  , cmdAliases = ["tr"]
  , cmdHelp = "Remove user-added taint propagators for a function"
  , cmdUsage = "taint-remove <func>"
  , cmdAction = taintRemoveAction
  }

taintRemoveAction :: ShellState -> [Text] -> IO CommandResult
taintRemoveAction _st [] = return $ ResultError "Usage: taint-remove <func>"
taintRemoveAction st (func : _) = do
  let isDefault = any (\(FunctionCallPropagator n _ _) -> n == func) LibC.taintPropagators
  old <- readIORef (st ^. #userTaintPropagators)
  let new = filter (\(FunctionCallPropagator n _ _) -> n /= func) old
      removed = length old - length new
  writeIORef (st ^. #userTaintPropagators) new
  if removed > 0
    then do
      bumpTaintConfigVersion st
      return $ ResultOk $ "Removed " <> show removed <> " propagator(s) for: " <> func
    else if isDefault
      then return $ ResultError $ func <> " is a default propagator and cannot be removed."
      else return $ ResultError $ "No user-added propagator found for: " <> func

-- ---------------------------------------------------------------------------
-- taint-reset
-- ---------------------------------------------------------------------------

taintResetCommand :: ShellCommand
taintResetCommand = ShellCommand
  { cmdName = "taint-reset"
  , cmdAliases = []
  , cmdHelp = "Clear all user-added taint propagators"
  , cmdUsage = "taint-reset"
  , cmdAction = taintResetAction
  }

taintResetAction :: ShellState -> [Text] -> IO CommandResult
taintResetAction st _args = do
  writeIORef (st ^. #userTaintPropagators) []
  bumpTaintConfigVersion st
  return $ ResultOk "Cleared all user-added taint propagators."
