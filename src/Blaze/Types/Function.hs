{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Function where

import Binja.Core (Address, InstructionIndex)
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
  , _dest :: Maybe (MLIL.Expression F)
  , _outputDest :: Maybe [SSAVariable]
  , _op :: CallOperation
  } deriving (Eq, Ord, Show)

-- TODO: the params returning single expr can probably be expanded
getParams :: MLIL.Operation (MLIL.Expression F) -> [MLIL.Expression F]
getParams (MLIL.CALL op) = op ^. MLIL.params
getParams (MLIL.CALL_SSA op) = op ^. MLIL.params
getParams (MLIL.CALL_UNTYPED op) = [op ^. MLIL.params]
getParams (MLIL.CALL_UNTYPED_SSA op) = [op ^. MLIL.params]
getParams (MLIL.TAILCALL op) = op ^. MLIL.params
getParams (MLIL.TAILCALL_SSA op) = op ^. MLIL.params
getParams (MLIL.TAILCALL_UNTYPED op) = [op ^. MLIL.params]
getParams (MLIL.TAILCALL_UNTYPED_SSA op) = [op ^. MLIL.params]
getParams (MLIL.SYSCALL op) = op ^. MLIL.params
getParams (MLIL.SYSCALL_SSA op) = op ^. MLIL.params
getParams (MLIL.SYSCALL_UNTYPED op) = [op ^. MLIL.params]
getParams (MLIL.SYSCALL_UNTYPED_SSA op) = [op ^. MLIL.params]
getParams _ = []

getOutputDest :: MLIL.Expression F -> Maybe [SSAVariable]
getOutputDest expr = case expr ^. MLIL.op of
  (MLIL.CALL_OUTPUT_SSA x) -> Just $ x ^. MLIL.dest
  _ -> Nothing

toCallInstruction :: MLIL.Instruction F -> Maybe CallInstruction
toCallInstruction instr = toCallInstr <$> case instr ^. MLIL.op of
  MLIL.CALL                 op -> Just (Just $ op ^. MLIL.dest, Nothing, CALL op)
  MLIL.CALL_SSA             op -> Just ( Just $ op ^. MLIL.dest
                                       , getOutputDest $ op ^. MLIL.output
                                       , CALL_SSA op )
  MLIL.CALL_UNTYPED         op -> Just ( Just $ op ^. MLIL.dest
                                       , getOutputDest $ op ^. MLIL.output
                                       , CALL_UNTYPED op )
  MLIL.CALL_UNTYPED_SSA     op -> Just ( Just $ op ^. MLIL.dest
                                       , getOutputDest $ op ^. MLIL.output
                                       , CALL_UNTYPED_SSA op )
  MLIL.TAILCALL             op -> Just (Just $ op ^. MLIL.dest, Nothing, TAILCALL op)
  MLIL.TAILCALL_SSA         op -> Just ( Just $ op ^. MLIL.dest
                                       , getOutputDest $ op ^. MLIL.output
                                       , TAILCALL_SSA op )
  MLIL.TAILCALL_UNTYPED     op -> Just ( Just $ op ^. MLIL.dest
                                       , getOutputDest $ op ^. MLIL.output
                                       , TAILCALL_UNTYPED op )
  MLIL.TAILCALL_UNTYPED_SSA op -> Just ( Just $ op ^. MLIL.dest
                                       , getOutputDest $ op ^. MLIL.output
                                       , TAILCALL_UNTYPED_SSA op )
  MLIL.SYSCALL              op -> Just (Nothing, Nothing, SYSCALL op)
  MLIL.SYSCALL_SSA          op -> Just ( Nothing
                                       , getOutputDest $ op ^. MLIL.output
                                       , SYSCALL_SSA op )
  MLIL.SYSCALL_UNTYPED      op -> Just ( Nothing
                                       , getOutputDest $ op ^. MLIL.output
                                       , SYSCALL_UNTYPED op )
  MLIL.SYSCALL_UNTYPED_SSA  op -> Just ( Nothing
                                       , getOutputDest $ op ^. MLIL.output
                                       , SYSCALL_UNTYPED_SSA op )
  _                            -> Nothing
  where
    toCallInstr (mdest', mOutputDest, op') = CallInstruction
      (instr ^. MLIL.address)
      (instr ^. MLIL.index)
      (instr ^. MLIL.size)
      (getParams $ instr ^. MLIL.op)
      mdest'
      mOutputDest
      op'
  

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
