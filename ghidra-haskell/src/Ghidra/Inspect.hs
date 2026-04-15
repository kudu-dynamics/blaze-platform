{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Rendering of Ghidra low/high IR at an address or across a function for
-- the @inspect_address@ and @dump_lift@ tools. Labels are supplied by the
-- caller (via 'InspectParams') so this module carries no hardcoded
-- backend-specific strings — the blaze-level 'BackendDescriptor' is the
-- source of truth and the call site unpacks it into the params.
module Ghidra.Inspect
  ( InspectStage (..)
  , InspectParams (..)
  , inspectAddress
  , dumpFunctionLift
  ) where

import Ghidra.Prelude

import qualified Ghidra.State as State
import Ghidra.State (GhidraState)
import qualified Ghidra.Instruction as Instr
import qualified Ghidra.BasicBlock as BB
import qualified Ghidra.Pcode as Pcode
import qualified Ghidra.Function as GFunc
import qualified Ghidra.Address as Addr
import qualified Ghidra.Types as J
import Ghidra.Types.Internal (Ghidra, runIO)
import qualified Language.Java as Java
import qualified Foreign.JNI as JNI

import qualified Data.BinaryAnalysis as BA
import qualified Data.Text as Text


-- | Which IR stage(s) to include. Mirrors the blaze-level
-- @Blaze.Import.Binary.Stage@ so this module stays free of a blaze
-- dependency (ghidra-haskell sits below blaze in the package graph).
data InspectStage = IStageLow | IStageHigh | IStageBoth
  deriving (Eq, Show, Generic)


-- | Parameters for rendering. The caller (blaze-level Ghidra instance)
-- unpacks its 'BackendDescriptor' into these fields and passes them in.
data InspectParams = InspectParams
  { lowLabel :: Text
  , highLabel :: Text
  , backendLabel :: Text
  , stage :: InspectStage
  }
  deriving (Eq, Show, Generic)


includesLow :: InspectStage -> Bool
includesLow IStageLow = True
includesLow IStageBoth = True
includesLow IStageHigh = False


includesHigh :: InspectStage -> Bool
includesHigh IStageHigh = True
includesHigh IStageBoth = True
includesHigh IStageLow = False


-- | Convert a Ghidra-internal address to the blaze-side 'BA.Address' used
-- for display and range comparisons.
toBA :: Addr.Address -> BA.Address
toBA a = BA.Address
  { BA.space = BA.AddressSpace
      { BA.ptrSize = a ^. #space . #ptrSize
      , BA.addressableUnitSize = a ^. #space . #addressableUnitSize
      , BA.name = a ^. #space . #name
      }
  , BA.offset = a ^. #offset
  }


-- | Render a 'BA.Address' for output headers without the @"Address "@
-- prefix baked into 'BA.Address''s 'Show' instance. We strip the prefix
-- rather than reinvent the formatter so that non-@Ram@ spaces still
-- surface their space name (the prefix is literal @"Address "@ for all
-- spaces in @Data.BinaryAnalysis@).
showAddrHex :: BA.Address -> Text
showAddrHex a =
  let s = Text.pack (show a)
  in fromMaybe s (Text.stripPrefix "Address " s)


renderPcodeOp :: forall op. Coercible op J.PcodeOp => op -> Ghidra Text
renderPcodeOp op = runIO $
  Java.call (coerce op :: J.PcodeOp) "toString"
    >>= JNI.newGlobalRef
    >>= Java.reify


renderInstrText :: J.Instruction -> Ghidra Text
renderInstrText instr = runIO $
  Java.call (coerce instr :: J.InstructionDB) "toString"
    >>= JNI.newGlobalRef
    >>= Java.reify


-- | Inspect the single instruction at an address. Returns @Nothing@ when
-- no instruction lives there; otherwise returns assembly + basic-block
-- range + the low and/or high IR per the stage in 'InspectParams'.
inspectAddress
  :: InspectParams
  -> GhidraState
  -> BA.Address
  -> Ghidra (Maybe Text)
inspectAddress params gs addr = do
  let prg = gs ^. #program
  jaddr <- State.mkAddress prg addr
  Instr.fromAddr prg jaddr >>= \case
    Nothing -> return Nothing
    Just instr -> do
      instrText <- renderInstrText instr
      instrAddrG <- Instr.getAddress instr >>= Addr.mkAddress
      let instrBA = toBA instrAddrG

      blocks <- BB.getCodeBlocks gs instr
      bbInfo <- forM blocks $ \block -> do
        bbStart <- BB.getStartAddress block
        bb <- BB.mkBasicBlock block
        bbEnd <- BB.getMaxAddress bb
        return $ "  block: " <> showAddrHex (toBA bbStart) <> " .. " <> showAddrHex (toBA bbEnd)

      lowSection <-
        if includesLow params.stage
          then renderLowSection params instr
          else return []

      highSection <-
        if includesHigh params.stage
          then renderHighSectionForInstruction params gs instr
          else return []

      let header =
            [ "Instruction at " <> showAddrHex instrBA <> ": " <> instrText
            , params.backendLabel <> " basic block(s):"
            ] <> bbInfo
      return . Just . Text.unlines $ header <> lowSection <> highSection


renderLowSection :: InspectParams -> J.Instruction -> Ghidra [Text]
renderLowSection params instr = do
  ops <- Pcode.getPcode instr
  opStrs <- traverse renderPcodeOp ops
  return $ "" : (params.lowLabel <> ":") : map ("  " <>) opStrs


renderHighSectionForInstruction
  :: InspectParams
  -> GhidraState
  -> J.Instruction
  -> Ghidra [Text]
renderHighSectionForInstruction params gs instr = do
  let prg = gs ^. #program
  jInstrAddr <- Instr.getAddress instr
  mFunc <- GFunc.fromAddr prg jInstrAddr
  case mFunc of
    Nothing ->
      return
        [ ""
        , params.highLabel <> ":"
        , "  (unavailable: no containing function at this address)"
        ]
    Just fn -> do
      hfunc <- GFunc.getHighFunction gs fn
      ops <- Pcode.getPcodeOpAST hfunc instr
      case ops of
        [] ->
          return
            [ ""
            , params.highLabel <> ":"
            , "  (no ops at this address — possibly pruned by decompiler)"
            ]
        _ -> do
          opStrs <- traverse renderPcodeOp ops
          return $ "" : (params.highLabel <> ":") : map ("  " <>) opStrs


-- | Dump the low and/or high IR for a whole function, identified by the
-- entry address (or any address inside it — resolution uses
-- @getFunctionContaining@). Optionally restricted to a contiguous
-- address range, inclusive on both ends. Instructions are rendered with
-- their low and/or high ops interleaved directly under each address so
-- the low→high gap is visible at a glance.
--
-- Returns @Left@ when no function can be resolved at the given address.
-- Decompile failures from 'GFunc.getHighFunction' propagate as exceptions
-- (they are @error@ calls in ghidra-haskell and are intentionally not
-- caught here).
dumpFunctionLift
  :: InspectParams
  -> GhidraState
  -> BA.Address
  -> Maybe (BA.Address, BA.Address)
  -> Ghidra (Either Text Text)
dumpFunctionLift params gs fnAddr mRange = do
  let prg = gs ^. #program
  jFnAddr <- State.mkAddress prg fnAddr
  mFunc <- GFunc.fromAddr prg jFnAddr
  case mFunc of
    Nothing ->
      return . Left $ "no function found containing address " <> show fnAddr
    Just fn -> do
      fnName <- GFunc.getName fn
      fnEntryG <- GFunc.getAddress fn
      let fnEntryBA = toBA fnEntryG

      allInstrs <- Instr.getInstructions prg fn
      taggedInstrs <- forM allInstrs $ \instr -> do
        ba <- toBA <$> (Instr.getAddress instr >>= Addr.mkAddress)
        return (ba, instr)
      let inRange ba = case mRange of
            Nothing -> True
            Just (lo, hi) -> ba >= lo && ba <= hi
          filtered = filter (inRange . fst) taggedInstrs

      mHfunc <-
        if includesHigh params.stage
          then Just <$> GFunc.getHighFunction gs fn
          else return Nothing

      chunks <- forM filtered $ \(ba, instr) -> do
        instrText <- renderInstrText instr
        lowChunk <-
          if includesLow params.stage
            then do
              ops <- Pcode.getPcode instr
              opStrs <- traverse renderPcodeOp ops
              return $ ("  " <> params.lowLabel <> ":") : map ("    " <>) opStrs
            else return []
        highChunk <- case (includesHigh params.stage, mHfunc) of
          (True, Just hf) -> do
            ops <- Pcode.getPcodeOpAST hf instr
            case ops of
              [] ->
                return
                  [ "  " <> params.highLabel <> ":"
                  , "    (no ops at this address — possibly pruned by decompiler)"
                  ]
              _ -> do
                opStrs <- traverse renderPcodeOp ops
                return $ ("  " <> params.highLabel <> ":") : map ("    " <>) opStrs
          _ -> return []
        let instrHeader = showAddrHex ba <> ": " <> instrText
        return $ [instrHeader] <> lowChunk <> highChunk <> [""]

      let banner =
            [ "=== " <> fnName <> " @ " <> showAddrHex fnEntryBA <> " ==="
            , "Backend: " <> params.backendLabel
            , ""
            ]
          rangeNote = case mRange of
            Nothing -> []
            Just (lo, hi) ->
              ["Range filter: " <> showAddrHex lo <> " .. " <> showAddrHex hi, ""]
      return . Right . Text.unlines $ banner <> rangeNote <> concat chunks
