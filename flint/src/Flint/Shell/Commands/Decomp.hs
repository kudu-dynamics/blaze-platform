module Flint.Shell.Commands.Decomp
  ( decompCommand
  ) where

import Flint.Prelude

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import Flint.Shell.Commands.Paths (findFunction)


decompCommand :: ShellCommand
decompCommand = ShellCommand
  { cmdName = "decomp"
  , cmdAliases = ["decompile"]
  , cmdHelp = "Decompile a function to C source"
  , cmdUsage = "decomp <func_name_or_addr>"
  , cmdAction = decompAction
  }

decompAction :: ShellState -> [Text] -> IO CommandResult
decompAction st [nameOrAddr] = case st ^. #decompFunc of
  Nothing -> return $ ResultError "Decompilation not available (no live backend)"
  Just decomp -> do
    mFunc <- findFunction st nameOrAddr
    case mFunc of
      Nothing -> return $ ResultError $ "Function not found: " <> nameOrAddr
      Just func -> do
        result <- decomp (func ^. #address)
        case result of
          Nothing -> return $ ResultError $ "Could not decompile: " <> nameOrAddr
          Just cSource -> return $ ResultText cSource
decompAction _ _ = return $ ResultError "Usage: decomp <func_name_or_addr>"
