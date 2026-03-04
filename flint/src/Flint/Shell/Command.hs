module Flint.Shell.Command where

import Flint.Prelude

import Flint.Shell.Types (ShellState, CommandResult(..))

import qualified Data.Text as Text


data ShellCommand = ShellCommand
  { cmdName    :: Text
  , cmdAliases :: [Text]
  , cmdHelp    :: Text
  , cmdUsage   :: Text
  , cmdAction  :: ShellState -> [Text] -> IO CommandResult
  } deriving (Generic)

findCommand :: [ShellCommand] -> Text -> Maybe ShellCommand
findCommand cmds input =
  find (\c -> c ^. #cmdName == input || input `elem` (c ^. #cmdAliases)) cmds

dispatchCommand :: [ShellCommand] -> ShellState -> Text -> IO CommandResult
dispatchCommand cmds st input = case Text.words input of
  [] -> return $ ResultOk ""
  (cmd : args) -> case findCommand cmds cmd of
    Nothing -> return $ ResultError $ "Unknown command: " <> cmd <> ". Type 'help' for available commands."
    Just c -> (c ^. #cmdAction) st args

formatHelp :: [ShellCommand] -> Text
formatHelp cmds = Text.unlines $
  ["Available commands:", ""] <>
  fmap formatCmd cmds <>
  [ ""
  , "  help                  Show this help message"
  , "  set solver on|off     Enable/disable the SMT solver"
  , "  quit / exit           Exit the shell"
  ]
  where
    formatCmd c =
      let aliases = case c ^. #cmdAliases of
            [] -> ""
            as -> " (" <> Text.intercalate ", " as <> ")"
          name = c ^. #cmdName <> aliases
      in "  " <> padRight 22 name <> c ^. #cmdHelp

padRight :: Int -> Text -> Text
padRight n t
  | Text.length t >= n = t <> " "
  | otherwise = t <> Text.replicate (n - Text.length t) " "
