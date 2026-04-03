module Flint.Shell.Commands.Functions
  ( functionsCommand
  ) where

import Flint.Prelude

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import qualified Flint.Cfg.Store as Store

import qualified Data.Text as Text


functionsCommand :: ShellCommand
functionsCommand = ShellCommand
  { cmdName = "functions"
  , cmdAliases = ["funcs", "lf"]
  , cmdHelp = "List functions [optional: -i (internal only), -e (extern only)] [filter]"
  , cmdUsage = "functions [-i|-e] [filter]"
  , cmdAction = listFunctions
  }

data FuncFilter = ShowAll | ShowInternal | ShowExtern

listFunctions :: ShellState -> [Text] -> IO CommandResult
listFunctions st args = do
  let (funcFilter, filterArgs) = case args of
        ("-i" : rest) -> (ShowInternal, rest)
        ("-e" : rest) -> (ShowExtern, rest)
        other         -> (ShowAll, other)
      filterText = case filterArgs of
        [] -> Nothing
        xs -> Just (Text.toLower $ Text.unwords xs)
      matchesFilter name = case filterText of
        Nothing -> True
        Just ft -> ft `Text.isInfixOf` Text.toLower name

  internals <- case funcFilter of
    ShowExtern -> return []
    _ -> do
      funcs <- Store.getInternalFuncs (st ^. #cfgStore)
      let filtered = filter (\f -> matchesFilter (f ^. #name)) funcs
          sorted = sortOn (view #address) filtered
      return $ fmap (\f -> (f ^. #name, f ^. #address)) sorted

  externs <- case funcFilter of
    ShowInternal -> return []
    _ -> do
      funcs <- Store.getExternalFuncs (st ^. #cfgStore)
      let filtered = filter (\f -> matchesFilter (f ^. #name)) funcs
          sorted = sortOn (view #name) filtered
      return $ fmap (\f -> (f ^. #name, Nothing)) sorted

  case (internals, externs) of
    ([], []) -> return $ ResultOk "No functions found."
    _        -> return $ ResultFunctions internals externs
