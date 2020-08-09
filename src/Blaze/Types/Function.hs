{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Function where

import Binja.Core (InstructionIndex)
import Binja.MLIL (Expression, OperationSize, SSAVariable)
import qualified Binja.MLIL as MLIL
import Blaze.Prelude
import Binja.Function (Function, MLILSSAFunction)

type F = MLILSSAFunction

data CallOperation = CALL (MLIL.CallOp (MLIL.Expression F))
                   | CALL_SSA (MLIL.CallSSAOp (MLIL.Expression F))
                   | CALL_UNTYPED (MLIL.CallUntypedOp (MLIL.Expression F))
                   | CALL_UNTYPED_SSA (MLIL.CallUntypedSSAOp (MLIL.Expression F))
                   | TAILCALL (MLIL.TailcallOp (MLIL.Expression F))
                   | TAILCALL_SSA (MLIL.TailcallSSAOp (MLIL.Expression F))
                   | TAILCALL_UNTYPED (MLIL.TailcallUntypedOp (MLIL.Expression F))
                   | TAILCALL_UNTYPED_SSA (MLIL.TailcallUntypedSSAOp (MLIL.Expression F))
                   | SYSCALL (MLIL.SyscallOp (MLIL.Expression F))
                   | SYSCALL_SSA (MLIL.SyscallSSAOp (MLIL.Expression F))
                   | SYSCALL_UNTYPED (MLIL.SyscallUntypedOp (MLIL.Expression F))
                   | SYSCALL_UNTYPED_SSA (MLIL.SyscallUntypedSSAOp (MLIL.Expression F))
                   deriving (Eq, Ord, Show)


data CallInstruction = CallInstruction
  { _address :: Address
  , _index :: InstructionIndex F
  , _size :: OperationSize
  , _params :: [MLIL.Expression F]
  , _dest :: Maybe (MLIL.Expression F) -- syscalls don't have a BNIL dest expression
  , _outputDest :: [SSAVariable]
  , _op :: CallOperation
  } deriving (Eq, Ord, Show)

getOutputDest :: MLIL.Expression F -> Maybe [SSAVariable]
getOutputDest expr = case expr ^. MLIL.op of
  (MLIL.CALL_OUTPUT_SSA x) -> Just $ x ^. MLIL.dest
  _ -> Nothing

toCallInstruction :: MLIL.Instruction F -> Maybe CallInstruction
toCallInstruction instr = toCallInstr <$> case instr ^. MLIL.op of
  MLIL.CALL_SSA op ->
    Just
      ( Just $ op ^. MLIL.dest,
        getOutputDest $ op ^. MLIL.output,
        CALL_SSA op
      )
  MLIL.CALL_UNTYPED_SSA op ->
    Just
      ( Just $ op ^. MLIL.dest,
        getOutputDest $ op ^. MLIL.output,
        CALL_UNTYPED_SSA op
      )
  MLIL.TAILCALL_SSA op ->
    Just
      ( Just $ op ^. MLIL.dest,
        getOutputDest $ op ^. MLIL.output,
        TAILCALL_SSA op
      )
  MLIL.TAILCALL_UNTYPED_SSA op ->
    Just
      ( Just $ op ^. MLIL.dest,
        getOutputDest $ op ^. MLIL.output,
        TAILCALL_UNTYPED_SSA op
      )
  MLIL.SYSCALL_SSA op ->
    Just
      ( Nothing,
        getOutputDest $ op ^. MLIL.output,
        SYSCALL_SSA op
      )
  MLIL.SYSCALL_UNTYPED_SSA op ->
    Just
      ( Nothing,
        getOutputDest $ op ^. MLIL.output,
        SYSCALL_UNTYPED_SSA op
      )
  _ -> Nothing
  where
    toCallInstr (mdest', mOutputDest, op') =
      case mOutputDest of
        (Just outputDest) ->
          CallInstruction
            (instr ^. MLIL.address)
            (instr ^. MLIL.index)
            (instr ^. MLIL.size)
            (MLIL.getParams $ instr ^. MLIL.op)
            mdest'
            outputDest
            op'
        Nothing ->
          error "SSA call without output destinations (returned results)."

  

data DestCollOpt = DestCollAddr Address
                 | DestCollExpr (Expression F)
                 deriving (Eq, Ord, Show)

data CallDest = DestAddr Address
              | DestFunc Function
              | DestExpr (Expression F)
              | DestColl (Set DestCollOpt)
              deriving (Eq, Ord, Show)

data CallSite = CallSite
  { _caller :: Function
  , _callInstr :: CallInstruction
  , _callDest :: CallDest
  } deriving (Eq, Ord, Show)

-- data RetInstruction = RetInstruction
--   { _address :: Address
--   , _index :: InstructionIndex F
--   , _}

$(makeFieldsNoPrefix ''CallInstruction)
$(makeFieldsNoPrefix ''CallSite)
