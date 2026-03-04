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
  , cmdHelp = "List functions [optional filter substring]"
  , cmdUsage = "functions [filter]"
  , cmdAction = listFunctions
  }

listFunctions :: ShellState -> [Text] -> IO CommandResult
listFunctions st args = do
  funcs <- Store.getInternalFuncs (st ^. #cfgStore)
  let filterText = case args of
        [] -> Nothing
        xs -> Just (Text.toLower $ Text.unwords xs)
      filtered = case filterText of
        Nothing -> funcs
        Just ft -> filter (\f -> ft `Text.isInfixOf` Text.toLower (f ^. #name)) funcs
      sorted = sortOn (view #address) filtered
      results = fmap (\f -> (f ^. #name, f ^. #address)) sorted
  case results of
    [] -> return $ ResultOk "No functions found."
    _  -> return $ ResultFunctions results
