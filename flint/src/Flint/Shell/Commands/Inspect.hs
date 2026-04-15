module Flint.Shell.Commands.Inspect
  ( inspectCommand
  , saveCommand
  , parseStage
  ) where

import Flint.Prelude

import Blaze.Import.Binary (Stage (..))

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import Flint.Shell.Commands.Paths (parseAddress)

import qualified Data.Text as Text


-- | Parse a @stage=X@ argument token. Returns 'Nothing' on an unrecognized
-- value so callers can surface a clear error.
parseStage :: Text -> Maybe Stage
parseStage t = case Text.toLower t of
  "low"  -> Just StageLow
  "high" -> Just StageHigh
  "both" -> Just StageBoth
  _      -> Nothing


-- | Pluck a @key=value@ token out of an argument list. Leaves other tokens
-- in place.
takeKeyArg :: Text -> [Text] -> (Maybe Text, [Text])
takeKeyArg key = go []
  where
    prefix = key <> "="
    go acc [] = (Nothing, reverse acc)
    go acc (t:ts) = case Text.stripPrefix prefix t of
      Just v  -> (Just v, reverse acc <> ts)
      Nothing -> go (t:acc) ts


inspectCommand :: ShellCommand
inspectCommand = ShellCommand
  { cmdName = "inspect"
  , cmdAliases = ["ins"]
  , cmdHelp = "Inspect the instruction and IR at a given address (stage=low|high|both, default both)"
  , cmdUsage = "inspect <addr> [stage=low|high|both]"
  , cmdAction = inspectAddr'
  }

inspectAddr' :: ShellState -> [Text] -> IO CommandResult
inspectAddr' st args = case st ^. #inspectAddr of
  Nothing -> return $ ResultError "Inspect not available (no live backend)"
  Just inspect ->
    let (mStageText, rest) = takeKeyArg "stage" args
    in case rest of
      [addrText] -> case parseAddress addrText of
        Nothing -> return $ ResultError $ "Invalid address: " <> addrText
        Just addr -> case maybe (Just StageBoth) parseStage mStageText of
          Nothing -> return $ ResultError $
            "Invalid stage (expected low|high|both): " <> fromMaybe "" mStageText
          Just stage -> do
            let addr' = addr & #space .~ (st ^. #baseOffset . #space)
            inspect addr' stage >>= \case
              Nothing -> return $ ResultOk $ "No instruction at " <> addrText
              Just result -> return $ ResultText result
      _ -> return $ ResultError "Usage: inspect <addr> [stage=low|high|both]"

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
