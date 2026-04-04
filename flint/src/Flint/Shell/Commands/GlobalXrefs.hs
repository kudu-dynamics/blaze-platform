module Flint.Shell.Commands.GlobalXrefs
  ( globalXrefsCommand
  ) where

import Flint.Prelude

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))

import Blaze.Import.Xref (Xref)

import qualified Data.Text as Text
import Numeric (showHex, readHex)


globalXrefsCommand :: ShellCommand
globalXrefsCommand = ShellCommand
  { cmdName = "global-xrefs"
  , cmdAliases = ["gxrefs"]
  , cmdHelp = "Find functions that reference a global address or named symbol"
  , cmdUsage = "global-xrefs <0xAddr | symbol_name>"
  , cmdAction = globalXrefsAction
  }

globalXrefsAction :: ShellState -> [Text] -> IO CommandResult
globalXrefsAction st args = case st ^. #xrefsTo of
  Nothing -> return $ ResultError "global-xrefs not available (no live backend)"
  Just queryXrefs -> case args of
    [] -> return $ ResultError
      "Usage: global-xrefs <0xAddr | symbol_name>\n\
      \  e.g. global-xrefs 0x604020\n\
      \       global-xrefs CFG_FILE"
    _ -> do
      resolved <- resolveTarget st args
      case resolved of
        Left err -> return $ ResultError err
        Right (target, label) -> do
          xrefs <- queryXrefs target
          case xrefs of
            [] -> return . ResultOk $ "No references found for " <> label
            _ -> return . ResultText . Text.unlines $ fmap formatXref xrefs

-- | Resolve the target to an Address.
-- Tries hex address first, then falls back to symbol name lookup.
resolveTarget :: ShellState -> [Text] -> IO (Either Text (Address, Text))
resolveTarget st args = case args of
  [arg] | Just addr <- parseHexAddr arg -> do
    let addrSpace = st ^. #baseOffset . #space
        target = intToAddr addr & #space .~ addrSpace
    return . Right $ (target, showHexAddr addr)
  _ -> do
    let name = Text.unwords args
    case st ^. #lookupSymbol of
      Nothing -> return $ Left $ "Not a hex address and symbol lookup not available: " <> name
      Just lookupSym -> do
        mAddr <- lookupSym name
        case mAddr of
          Nothing -> return $ Left $ "Symbol not found: " <> name
          Just addr -> return . Right $ (addr, name <> " (" <> showAddr addr <> ")")

formatXref :: Xref -> Text
formatXref xref =
  let fName = xref ^. #function . #name
      xrefAddr = xref ^. #address
  in fName <> " @ " <> showAddr xrefAddr

showHexAddr :: Int64 -> Text
showHexAddr n = "0x" <> Text.pack (showHex (fromIntegral n :: Word64) "")

showAddr :: Address -> Text
showAddr addr = "0x" <> Text.pack (showHex (addrToInt addr) "")

parseHexAddr :: Text -> Maybe Int64
parseHexAddr t =
  let hex = fromMaybe t $ Text.stripPrefix "0x" t
  in case readHex (Text.unpack hex) of
    [(n, "")] -> Just n
    _         -> Nothing
