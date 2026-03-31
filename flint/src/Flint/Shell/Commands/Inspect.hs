module Flint.Shell.Commands.Inspect
  ( inspectCommand
  , saveCommand
  ) where

import Flint.Prelude

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import Flint.Shell.Commands.Paths (parseAddress)



inspectCommand :: ShellCommand
inspectCommand = ShellCommand
  { cmdName = "inspect"
  , cmdAliases = ["ins"]
  , cmdHelp = "Inspect the raw instruction and P-code at a given address"
  , cmdUsage = "inspect <addr>"
  , cmdAction = inspectAddr'
  }

inspectAddr' :: ShellState -> [Text] -> IO CommandResult
inspectAddr' st [addrText] = case st ^. #inspectAddr of
  Nothing -> return $ ResultError "Inspect not available (no live backend)"
  Just inspect -> case parseAddress addrText of
    Nothing -> return $ ResultError $ "Invalid address: " <> addrText
    Just addr -> do
      let addr' = addr & #space .~ (st ^. #baseOffset . #space)
      inspect addr' >>= \case
        Nothing -> return $ ResultOk $ "No instruction at " <> addrText
        Just result -> return $ ResultText result
inspectAddr' _ _ = return $ ResultError "Usage: inspect <addr>"

saveCommand :: ShellCommand
saveCommand = ShellCommand
  { cmdName = "save"
  , cmdAliases = []
  , cmdHelp = "Save the current binary as a .gzf file"
  , cmdUsage = "save <filepath>"
  , cmdAction = saveDb'
  }

saveDb' :: ShellState -> [Text] -> IO CommandResult
saveDb' st [fpText] = case st ^. #saveToDb of
  Nothing -> return $ ResultError "Save not available (no live backend)"
  Just save -> save (cs fpText) >>= \case
    Left err -> return $ ResultError $ "Save failed: " <> err
    Right fp -> return $ ResultOk $ "Saved to " <> cs fp
saveDb' _ _ = return $ ResultError "Usage: save <filepath>"
