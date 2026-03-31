{-# LANGUAGE DataKinds #-}
module Ghidra.Inspect
  ( inspectAddress
  ) where

import Ghidra.Prelude

import qualified Ghidra.State as State
import Ghidra.State (GhidraState)
import qualified Ghidra.Instruction as Instr
import qualified Ghidra.BasicBlock as BB
import qualified Ghidra.Pcode as Pcode
import qualified Ghidra.Address as Addr
import qualified Ghidra.Types as J
import Ghidra.Types.Internal (Ghidra, runIO)
import qualified Language.Java as Java
import qualified Foreign.JNI as JNI

import qualified Data.BinaryAnalysis as BA
import qualified Data.Text as Text


-- | Inspect the raw instruction and P-code at a given address.
-- Returns the instruction text, containing basic block range, and raw P-code.
inspectAddress :: GhidraState -> BA.Address -> Ghidra (Maybe Text)
inspectAddress gs addr = do
  let prg = gs ^. #program
  jaddr <- State.mkAddress prg addr
  Instr.fromAddr prg jaddr >>= \case
    Nothing -> return Nothing
    Just instr -> do
      -- Get instruction text representation
      instrText :: Text <- runIO $ Java.call (coerce instr :: J.InstructionDB) "toString" >>= JNI.newGlobalRef >>= Java.reify

      -- Get the instruction's actual address (may differ from queried addr if mid-instruction)
      instrAddr <- Instr.getAddress instr >>= Addr.mkAddress

      -- Get the Ghidra basic block containing this instruction
      blocks <- BB.getCodeBlocks gs instr
      bbInfo <- forM blocks $ \block -> do
        bbStart <- BB.getStartAddress block
        bb <- BB.mkBasicBlock block
        bbEnd <- BB.getMaxAddress bb
        return $ "  block: " <> show (toBA bbStart) <> " .. " <> show (toBA bbEnd)

      -- Get raw P-code ops for this instruction
      pcodeOps <- Pcode.getPcode instr
      pcodeStrs <- forM pcodeOps $ \op -> do
        opStr :: Text <- runIO $ Java.call (coerce op :: J.PcodeOp) "toString" >>= JNI.newGlobalRef >>= Java.reify
        return $ "  " <> (opStr :: Text)

      let result = Text.unlines $
            [ "Instruction at " <> show (toBA instrAddr) <> ": " <> instrText
            , "Ghidra basic block(s):"
            ] <> bbInfo <>
            [ "Raw P-code:" ] <> pcodeStrs
      return $ Just result
  where
    toBA :: Addr.Address -> BA.Address
    toBA a = BA.Address
      { BA.space = BA.AddressSpace
        { BA.ptrSize = a ^. #space . #ptrSize
        , BA.addressableUnitSize = a ^. #space . #addressableUnitSize
        , BA.name = a ^. #space . #name
        }
      , BA.offset = a ^. #offset
      }
