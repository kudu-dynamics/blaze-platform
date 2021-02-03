-- todo: most this to binja converter dir
module Blaze.Types.Function where

import Blaze.Prelude hiding (Symbol)

import Binja.Core (InstructionIndex)
import Binja.MLIL (Expression, OperationSize, SSAVariable)
import qualified Binja.MLIL as MLIL
import Data.BinaryAnalysis (Symbol (Symbol))
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
                   deriving (Eq, Ord, Show, Generic)
                   deriving anyclass Hashable


data CallInstruction = CallInstruction
  { instr :: MLIL.Instruction F
  , address :: Address
  , index :: InstructionIndex F
  , size :: OperationSize
  , params :: [MLIL.Expression F]
  , dest :: Maybe (MLIL.Expression F) -- syscalls don't have a BNIL dest expression
  , outputDest :: [SSAVariable]
  , op :: CallOperation
  } 
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass Hashable

getOutputDest :: MLIL.Expression F -> Maybe [SSAVariable]
getOutputDest expr = case expr ^. MLIL.op of
  (MLIL.CALL_OUTPUT_SSA x) -> Just $ x ^. MLIL.dest
  _ -> Nothing

toCallInstruction :: MLIL.Instruction F -> Maybe CallInstruction
toCallInstruction inst = toCallInstr <$> case inst ^. MLIL.op of
  MLIL.CALL_SSA op' ->
    Just
      ( Just $ op' ^. MLIL.dest,
        getOutputDest $ op' ^. MLIL.output,
        CALL_SSA op'
      )
  MLIL.CALL_UNTYPED_SSA op' ->
    Just
      ( Just $ op' ^. MLIL.dest,
        getOutputDest $ op' ^. MLIL.output,
        CALL_UNTYPED_SSA op'
      )
  MLIL.TAILCALL_SSA op' ->
    Just
      ( Just $ op' ^. MLIL.dest,
        getOutputDest $ op' ^. MLIL.output,
        TAILCALL_SSA op'
      )
  MLIL.TAILCALL_UNTYPED_SSA op' ->
    Just
      ( Just $ op' ^. MLIL.dest,
        getOutputDest $ op' ^. MLIL.output,
        TAILCALL_UNTYPED_SSA op'
      )
  MLIL.SYSCALL_SSA op' ->
    Just
      ( Nothing,
        getOutputDest $ op' ^. MLIL.output,
        SYSCALL_SSA op'
      )
  MLIL.SYSCALL_UNTYPED_SSA op' ->
    Just
      ( Nothing,
        getOutputDest $ op' ^. MLIL.output,
        SYSCALL_UNTYPED_SSA op'
      )
  _ -> Nothing
  where
    toCallInstr (mdest', mOutputDest, op') =
      case mOutputDest of
        (Just outputDest') ->
          CallInstruction
            inst
            (inst ^. MLIL.address)
            (inst ^. MLIL.index)
            (inst ^. MLIL.size)
            (MLIL.getParams $ inst ^. MLIL.op)
            mdest'
            outputDest'
            op'
        Nothing ->
          error "SSA call without output destinations (returned results)."

  

data DestCollOpt = DestCollAddr Address
                 | DestCollExpr (Expression F)
                 deriving (Eq, Ord, Show, Generic)

data CallDest = DestAddr Address
              | DestFunc Function
              | DestExpr (Expression F)
              | DestColl (Set DestCollOpt)
              deriving (Eq, Ord, Show, Generic)

data CallSite = CallSite
  { caller :: Function
  , callInstr :: CallInstruction
  , callDest :: CallDest
  } deriving (Eq, Ord, Show, Generic)

-- data RetInstruction = RetInstruction
--   { _address :: Address
--   , _index :: InstructionIndex F
--   , _}

data Access = In | Out | InOut | Unknown
  deriving (Enum, Eq, Show, Generic)
  deriving anyclass Hashable

data ParamInfo
  = ParamInfo
      { name :: Text,
        access :: Access
      }
  deriving (Eq, Show, Generic)
  deriving anyclass Hashable

data FuncParamInfo
  = FuncParamInfo ParamInfo
  | FuncVarArgInfo
  deriving (Eq, Show, Generic)
  deriving anyclass Hashable

newtype ResultInfo
  = ResultInfo
      { name :: Text }
  deriving (Eq, Show, Generic)
  deriving anyclass Hashable

-- | Describe basic information about a function.
-- Because this describe known functions, we are matching
-- according to symbol. This will work for C, but not C++.
-- Can try matching according to simple name from Symbol and
-- parameter list.
data FuncInfo
  = FuncInfo
      { name :: Symbol,
        params :: [FuncParamInfo],
        result :: ResultInfo
      }
  deriving (Eq, Show, Generic)
  deriving anyclass Hashable

mkFuncInfo :: Text -> Text -> [FuncParamInfo] -> ResultInfo -> FuncInfo
mkFuncInfo name' rawName =
  FuncInfo (Symbol name' rawName)
