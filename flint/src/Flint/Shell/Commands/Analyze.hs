module Flint.Shell.Commands.Analyze
  ( analyzeAllCommand
  ) where

import Flint.Prelude

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import qualified Flint.Cfg.Store as Store


analyzeAllCommand :: ShellCommand
analyzeAllCommand = ShellCommand
  { cmdName = "analyze-all"
  , cmdAliases = ["aa"]
  , cmdHelp = "Pre-analyze all functions (decompile + build CFGs). Persists to DB."
  , cmdUsage = "analyze-all"
  , cmdAction = doAnalyzeAll
  }

doAnalyzeAll :: ShellState -> [Text] -> IO CommandResult
doAnalyzeAll st _args = do
  n <- Store.analyzeAll (st ^. #cfgStore)
  return . ResultOk $ "Analyzed " <> show n <> " functions."
