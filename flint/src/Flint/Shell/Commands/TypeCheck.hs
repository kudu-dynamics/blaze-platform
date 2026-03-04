module Flint.Shell.Commands.TypeCheck
  ( typecheckCommand
  ) where

import Flint.Prelude

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))


typecheckCommand :: ShellCommand
typecheckCommand = ShellCommand
  { cmdName = "typecheck"
  , cmdAliases = ["tc"]
  , cmdHelp = "Type-check a path (not yet implemented)"
  , cmdUsage = "typecheck <path_id>"
  , cmdAction = \_st _args -> return $ ResultError "typecheck is not yet implemented."
  }
