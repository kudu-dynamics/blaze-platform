module Flint.Shell.Commands.Strings
  ( stringsCommand
  , stringXrefsCommand
  ) where

import Flint.Prelude

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))

import Blaze.Import.Xref (Xref)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text as Text
import Numeric (showHex, readHex)


stringsCommand :: ShellCommand
stringsCommand = ShellCommand
  { cmdName = "strings"
  , cmdAliases = []
  , cmdHelp = "List strings in the binary. No arg = all, \"text\" = filter by content, 0xAddr = lookup by address."
  , cmdUsage = "strings [\"filter\" | 0xAddr]"
  , cmdAction = stringsAction
  }

stringXrefsCommand :: ShellCommand
stringXrefsCommand = ShellCommand
  { cmdName = "string-xrefs"
  , cmdAliases = ["sxrefs"]
  , cmdHelp = "Find functions referencing a string. 0xAddr = single string, \"text\" = all matching strings."
  , cmdUsage = "string-xrefs <0xAddr | \"filter\">"
  , cmdAction = stringXrefsAction
  }

stringsAction :: ShellState -> [Text] -> IO CommandResult
stringsAction st args = do
  let smap = st ^. #cfgStore . #stringsMap
  case parseStringArg args of
    Nothing ->
      -- No arg: show all strings sorted by address
      return . ResultText . formatStrings $ sortedEntries smap
    Just (Left addr) ->
      -- Bare hex: lookup specific address
      case HashMap.lookup addr smap of
        Nothing -> return $ ResultOk $ "No string at address " <> showAddr addr
        Just s  -> return . ResultText $ formatStringEntry addr s
    Just (Right filterText) ->
      -- Quoted string: filter by content
      let matches = filter (\(_, s) -> Text.toLower filterText `Text.isInfixOf` Text.toLower s)
                  $ sortedEntries smap
      in case matches of
        [] -> return $ ResultOk $ "No strings matching: " <> filterText
        _  -> return . ResultText . formatStrings $ matches

stringXrefsAction :: ShellState -> [Text] -> IO CommandResult
stringXrefsAction st args = do
  let store = st ^. #cfgStore
      smap = store ^. #stringsMap
      sxrefs = store ^. #stringXrefs
  case parseStringArg args of
    Nothing -> return $ ResultError "Usage: string-xrefs <0xAddr | \"filter\">"
    Just (Left addr) ->
      -- Single address xref
      case HashMap.lookup addr sxrefs of
        Nothing -> return $ ResultOk $ "No xrefs found for address " <> showAddr addr
        Just [] -> return $ ResultOk $ "No xrefs found for address " <> showAddr addr
        Just xrefs -> return . ResultText . Text.unlines $ fmap formatXref xrefs
    Just (Right filterText) -> do
      -- Filter strings, then show xrefs for all matches
      let matches = filter (\(_, s) -> Text.toLower filterText `Text.isInfixOf` Text.toLower s)
                  $ sortedEntries smap
      case matches of
        [] -> return $ ResultOk $ "No strings matching: " <> filterText
        _ -> do
          let sections = flip concatMap matches $ \(addr, s) ->
                let xrefs = fromMaybe [] $ HashMap.lookup addr sxrefs
                    header = "String " <> showAddr addr <> ": " <> "\"" <> s <> "\""
                in if null xrefs
                   then [header, "  (no xrefs)"]
                   else header : fmap (\x -> "  " <> formatXref x) xrefs
          return . ResultText . Text.unlines $ sections

-- | Parse the argument to strings/string-xrefs commands.
-- Returns Left Address for bare hex, Right Text for quoted filter, Nothing for no args.
parseStringArg :: [Text] -> Maybe (Either Address Text)
parseStringArg [] = Nothing
parseStringArg args =
  let joined = Text.unwords args
  in if isQuoted joined
     then Just . Right . Text.drop 1 . Text.dropEnd 1 $ joined
     else Just . Left . intToAddr =<< parseHexAddr joined

isQuoted :: Text -> Bool
isQuoted t = (Text.isPrefixOf "\"" t && Text.isSuffixOf "\"" t)
          || (Text.isPrefixOf "'" t && Text.isSuffixOf "'" t)

parseHexAddr :: Text -> Maybe Int64
parseHexAddr t =
  let hex = fromMaybe t $ Text.stripPrefix "0x" t
  in case readHex (Text.unpack hex) of
    [(n, "")] -> Just n
    _         -> Nothing

sortedEntries :: HashMap Address Text -> [(Address, Text)]
sortedEntries = List.sortOn fst . HashMap.toList

formatStrings :: [(Address, Text)] -> Text
formatStrings = Text.unlines . fmap (uncurry formatStringEntry)

formatStringEntry :: Address -> Text -> Text
formatStringEntry addr s = showAddr addr <> ": \"" <> s <> "\""

formatXref :: Xref -> Text
formatXref xref =
  let funcName = xref ^. #function . #name
      xrefAddr = xref ^. #address
  in funcName <> " @ " <> showAddr xrefAddr

showAddr :: Address -> Text
showAddr addr = "0x" <> Text.pack (showHex (addrToInt addr) "")
