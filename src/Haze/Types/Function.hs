{-# LANGUAGE TemplateHaskell #-}

module Haze.Types.Function where

import Hinja.Core (Address, InstructionIndex)
import Hinja.Function (Function)
import Hinja.MLIL (Expression, OperationSize)
import qualified Hinja.MLIL as MLIL
import Haze.Prelude

data CallOperation = CALL (MLIL.CallOp F)
                   | CALL_SSA (MLIL.CallSSAOp F)
                   | CALL_UNTYPED (MLIL.CallUntypedOp F)
                   | CALL_UNTYPED_SSA (MLIL.CallUntypedSSAOp F)
                   | TAILCALL (MLIL.TailcallOp F)
                   | TAILCALL_SSA (MLIL.TailcallSSAOp F)
                   | TAILCALL_UNTYPED (MLIL.TailcallUntypedOp F)
                   | TAILCALL_UNTYPED_SSA (MLIL.TailcallUntypedSSAOp F)
                   | SYSCALL (MLIL.SyscallOp F)
                   | SYSCALL_SSA (MLIL.SyscallSSAOp F)
                   | SYSCALL_UNTYPED (MLIL.SyscallUntypedOp F)
                   | SYSCALL_UNTYPED_SSA (MLIL.SyscallUntypedSSAOp F)
                   deriving (Eq, Ord, Show)

                   
data CallInstruction = CallInstruction
  { _address :: Address
  , _index :: InstructionIndex F
  , _size :: OperationSize
  , _op :: CallOperation
  } deriving (Eq, Ord, Show)

toCallInstruction :: MLIL.Instruction F -> Maybe CallInstruction
toCallInstruction instr = toCallInstr <$> case instr ^. MLIL.op of
  MLIL.CALL                 op -> Just $ CALL op
  MLIL.CALL_SSA             op -> Just $ CALL_SSA op
  MLIL.CALL_UNTYPED         op -> Just $ CALL_UNTYPED op
  MLIL.CALL_UNTYPED_SSA     op -> Just $ CALL_UNTYPED_SSA op
  MLIL.TAILCALL             op -> Just $ TAILCALL op
  MLIL.TAILCALL_SSA         op -> Just $ TAILCALL_SSA op
  MLIL.TAILCALL_UNTYPED     op -> Just $ TAILCALL_UNTYPED op
  MLIL.TAILCALL_UNTYPED_SSA op -> Just $ TAILCALL_UNTYPED_SSA op
  MLIL.SYSCALL              op -> Just $ SYSCALL op
  MLIL.SYSCALL_SSA          op -> Just $ SYSCALL_SSA op
  MLIL.SYSCALL_UNTYPED      op -> Just $ SYSCALL_UNTYPED op
  MLIL.SYSCALL_UNTYPED_SSA  op -> Just $ SYSCALL_UNTYPED_SSA op
  _                            -> Nothing
  where
    toCallInstr = CallInstruction
      (instr ^. MLIL.address)
      (instr ^. MLIL.index)
      (instr ^. MLIL.size)
  

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

$(makeFieldsNoPrefix ''CallInstruction)
$(makeFieldsNoPrefix ''CallSite)
