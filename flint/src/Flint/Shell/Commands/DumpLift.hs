module Flint.Shell.Commands.DumpLift
  ( dumpLiftCommand
  ) where

import Flint.Prelude

import Blaze.Import.Binary (Stage (..))

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import Flint.Shell.Commands.Inspect (parseStage)
import Flint.Shell.Commands.Paths (findFunction, parseAddress)

import qualified Data.Text as Text


-- | Pluck a @key=value@ token out of an argument list. Leaves other tokens
-- in place. (Local copy to keep this module self-contained.)
takeKeyArg :: Text -> [Text] -> (Maybe Text, [Text])
takeKeyArg key = go []
  where
    prefix = key <> "="
    go acc [] = (Nothing, reverse acc)
    go acc (t:ts) = case Text.stripPrefix prefix t of
      Just v  -> (Just v, reverse acc <> ts)
      Nothing -> go (t:acc) ts


-- | Parse an @addresses=LO-HI@ argument value. The two halves must each
-- parse as hex/decimal addresses via 'parseAddress'.
parseRange :: Text -> Maybe (Address, Address)
parseRange t = case Text.splitOn "-" t of
  [loT, hiT] -> (,) <$> parseAddress loT <*> parseAddress hiT
  _          -> Nothing


dumpLiftCommand :: ShellCommand
dumpLiftCommand = ShellCommand
  { cmdName = "dump-lift"
  , cmdAliases = ["dumplift"]
  , cmdHelp =
      "Dump the low and/or high IR for an entire function. \
      \stage=low|high|both (default both). \
      \addresses=0xSTART-0xEND restricts output to a contiguous range."
  , cmdUsage = "dump-lift <func> [stage=low|high|both] [addresses=0xSTART-0xEND]"
  , cmdAction = dumpLift'
  }


dumpLift' :: ShellState -> [Text] -> IO CommandResult
dumpLift' st args = case st ^. #dumpLift of
  Nothing -> return $ ResultError "dump-lift not available (no live backend)"
  Just runDump ->
    let (mStageText, rest1) = takeKeyArg "stage" args
        (mRangeText, rest2) = takeKeyArg "addresses" rest1
    in case rest2 of
      [fnArg] -> do
        mFunc <- findFunction st fnArg
        case mFunc of
          Nothing -> return $ ResultError $ "Function not found: " <> fnArg
          Just fn ->
            case maybe (Just StageBoth) parseStage mStageText of
              Nothing -> return $ ResultError $
                "Invalid stage (expected low|high|both): "
                  <> fromMaybe "" mStageText
              Just stage -> case mRangeText of
                Just rt | isNothing (parseRange rt) ->
                  return $ ResultError $
                    "Invalid addresses range (expected LO-HI): " <> rt
                _ -> do
                  let mRange = mRangeText >>= parseRange
                      fnAddr = fn ^. #address
                      baseSpace = st ^. #baseOffset . #space
                      normalizeSpace a = a & #space .~ baseSpace
                      fnAddr' = normalizeSpace fnAddr
                      mRange' = fmap (bimap normalizeSpace normalizeSpace) mRange
                  runDump fnAddr' stage mRange' >>= \case
                    Left err -> return $ ResultError $ "dump-lift failed: " <> err
                    Right result -> return $ ResultText result
      _ -> return $ ResultError
        "Usage: dump-lift <func> [stage=low|high|both] [addresses=0xSTART-0xEND]"


