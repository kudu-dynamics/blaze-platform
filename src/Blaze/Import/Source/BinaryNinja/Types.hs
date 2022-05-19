module Blaze.Import.Source.BinaryNinja.Types where

import qualified Binja.BasicBlock as BNBb
import qualified Binja.Core as Binja
import qualified Binja.Function as BNFunc
import qualified Binja.MLIL as Mlil
import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Cfg (CfEdge, CfNode, CodeReference, NodeRefMap, NodeRefMapEntry, PilNode)
import Blaze.Types.Pil (
  CallDest,
  Expression,
 )
import qualified Blaze.Types.Pil as Pil
import Control.Monad.Trans.Writer.Lazy (WriterT)
import Data.DList (DList)

---- CallGraph
type MLILSSAOp = Mlil.Operation (Mlil.Expression BNFunc.MLILSSAFunction)

----- CFG
type MlilSsaFunc = BNFunc.MLILSSAFunction
type MlilSsaBlock = BNBb.BasicBlock MlilSsaFunc
type MlilSsaBlockEdge = BNBb.BlockEdge MlilSsaFunc
type MlilSsaInstruction = Mlil.Instruction MlilSsaFunc
type MlilSsaInstructionIndex = Binja.InstructionIndex MlilSsaFunc
type MlilSsaCfNode = CfNode (NonEmpty MlilSsaInstruction)
type MlilSsaCfEdge = CfEdge MlilSsaCfNode
type MlilSsaBlockMap = HashMap MlilSsaBlock [MlilSsaCfNode]

newtype NonCallInstruction = NonCallInstruction
  {unNonCallInstruction :: MlilSsaInstruction}
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

data InstrGroup
  = SingleCall {_callInstr :: CallInstruction}
  | ManyNonCalls {_nonCallInstrs :: NonEmpty NonCallInstruction}
  deriving (Eq, Ord, Show, Generic)

data MlilSsaInstr
  = MlilSsaCall CallInstruction
  | MlilSsaNonCall NonCallInstruction
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

type MlilCodeReference = CodeReference MlilSsaInstructionIndex
type MlilNode = CfNode (NonEmpty MlilSsaInstruction)
type MlilNodeRefMap = NodeRefMap MlilNode MlilCodeReference
type MlilNodeRefMapEntry = NodeRefMapEntry MlilNode MlilCodeReference
type NodeConverter a = WriterT (DList MlilNodeRefMapEntry) IO a

type PilMlilNodeMap = HashMap PilNode MlilCodeReference

----- Pil

data SSAVariableRef = SSAVariableRef
  { var :: Mlil.SSAVariable
  , func :: BNFunc.Function
  , ctxId :: Pil.CtxId
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

type F = BNFunc.MLILSSAFunction

data CallOperation = CALL (Mlil.CallOp (Mlil.Expression F))
                   | CALL_SSA (Mlil.CallSSAOp (Mlil.Expression F))
                   | CALL_UNTYPED (Mlil.CallUntypedOp (Mlil.Expression F))
                   | CALL_UNTYPED_SSA (Mlil.CallUntypedSSAOp (Mlil.Expression F))
                   | TAILCALL (Mlil.TailcallOp (Mlil.Expression F))
                   | TAILCALL_SSA (Mlil.TailcallSSAOp (Mlil.Expression F))
                   | TAILCALL_UNTYPED (Mlil.TailcallUntypedOp (Mlil.Expression F))
                   | TAILCALL_UNTYPED_SSA (Mlil.TailcallUntypedSSAOp (Mlil.Expression F))
                   | SYSCALL (Mlil.SyscallOp (Mlil.Expression F))
                   | SYSCALL_SSA (Mlil.SyscallSSAOp (Mlil.Expression F))
                   | SYSCALL_UNTYPED (Mlil.SyscallUntypedOp (Mlil.Expression F))
                   | SYSCALL_UNTYPED_SSA (Mlil.SyscallUntypedSSAOp (Mlil.Expression F))
                   deriving (Eq, Ord, Show, Generic)
                   deriving anyclass Hashable

data CallInstruction = CallInstruction
  { instr :: Mlil.Instruction F
  , address :: Address
  , index :: Binja.InstructionIndex F
  , size :: Mlil.OperationSize
  , params :: [Mlil.Expression F]
  , dest :: Maybe (Mlil.Expression F) -- syscalls don't have a BNIL dest expression
  , outputDest :: [Mlil.SSAVariable]
  , op :: CallOperation
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass Hashable

toCallInstruction :: Mlil.Instruction F -> Maybe CallInstruction
toCallInstruction inst = toCallInstr <$> case inst ^. Mlil.op of
  Mlil.CALL_SSA op' ->
    Just
      ( Just $ op' ^. Mlil.dest,
        getOutputDest $ op' ^. Mlil.output,
        CALL_SSA op'
      )
  Mlil.CALL_UNTYPED_SSA op' ->
    Just
      ( Just $ op' ^. Mlil.dest,
        getOutputDest $ op' ^. Mlil.output,
        CALL_UNTYPED_SSA op'
      )
  Mlil.TAILCALL_SSA op' ->
    Just
      ( Just $ op' ^. Mlil.dest,
        getOutputDest $ op' ^. Mlil.output,
        TAILCALL_SSA op'
      )
  Mlil.TAILCALL_UNTYPED_SSA op' ->
    Just
      ( Just $ op' ^. Mlil.dest,
        getOutputDest $ op' ^. Mlil.output,
        TAILCALL_UNTYPED_SSA op'
      )
  Mlil.SYSCALL_SSA op' ->
    Just
      ( Nothing,
        getOutputDest $ op' ^. Mlil.output,
        SYSCALL_SSA op'
      )
  Mlil.SYSCALL_UNTYPED_SSA op' ->
    Just
      ( Nothing,
        getOutputDest $ op' ^. Mlil.output,
        SYSCALL_UNTYPED_SSA op'
      )
  _ -> Nothing
  where
    toCallInstr (mdest', mOutputDest, op') =
      case mOutputDest of
        (Just outputDest') ->
          CallInstruction
            inst
            (inst ^. Mlil.address)
            (inst ^. Mlil.index)
            (inst ^. Mlil.size)
            (Mlil.getParams $ inst ^. Mlil.op)
            mdest'
            outputDest'
            op'
        Nothing ->
          error "SSA call without output destinations (returned results)."
    getOutputDest :: Mlil.Expression F -> Maybe [Mlil.SSAVariable]
    getOutputDest expr = case expr ^. Mlil.op of
      (Mlil.CALL_OUTPUT_SSA x) -> Just $ x ^. Mlil.dest
      _ -> Nothing

-- TODO: Remove once path conversion is based on interprocedural CFGs
-- A temporary workaround to support path conversion which still accesses
-- BN directly. Soon paths will be retrieved through InterCfg and this can all be removed.
data CallSite = CallSite
  { caller :: BNFunc.Function
  , callInstr :: CallInstruction
  , callDest :: CallDest Expression
  } deriving (Eq, Ord, Show, Generic, Hashable)

