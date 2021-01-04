module Blaze.Import.Source.BinaryNinja.Types where

import qualified Binja.BasicBlock as BNBb
import qualified Binja.Core as Binja
import qualified Binja.Function as BNFunc
import qualified Binja.MLIL as Mlil
import Blaze.Prelude hiding (Symbol)
import Blaze.Types.CallGraph (Function)
import Blaze.Types.Cfg (CfNode)
import Blaze.Types.Function (CallInstruction, FuncInfo)
import Blaze.Types.Path.AlgaPath (AlgaPath)
import Blaze.Types.Pil
  ( Ctx
  , CtxIndex
  , PilVar
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

type MlilSsaBlockMap = HashMap MlilSsaBlock [CfNode]

type NonCallInstruction = MlilSsaInstruction

data InstrGroup
  = SingleCall {_callInstr :: CallInstruction}
  | ManyNonCalls {_nonCallInstrs :: NonEmpty NonCallInstruction}
  deriving (Eq, Ord, Show, Generic)

data MlilSsaInstr
  = MlilSsaCall CallInstruction
  | MlilSsaNonCall NonCallInstruction
  deriving (Eq, Ord, Show, Generic)

data CodeReference = CodeReference
  { function :: Function
  , startIndex :: MlilSsaInstructionIndex
  , endIndex :: MlilSsaInstructionIndex
  }
  deriving (Eq, Ord, Show, Generic)

type NodeMap = HashMap CfNode CodeReference

type NodeMapEntry = (CfNode, CodeReference)

type NodeConverter a = WriterT (DList NodeMapEntry) IO a


----- Pil

-- TODO: Conversions sometimes occur without need for
--       a path. Identify and refactor appropriately.
data ConverterState = ConverterState
  {
    -- | The path being converted.
    path :: AlgaPath
    -- | The maximum context ID used so far
  , ctxMaxIdx :: CtxIndex
    -- | The current context should be on the top of the stack.
    -- I.e., the stack should never be empty.
  , ctxStack :: NonEmpty Ctx
    -- | The current context
  , ctx :: Ctx
    -- | Currently known defined PilVars for all contexts.
    -- This is assumed to be ordered by most recently defined first.
    -- TODO: Can we safeguard for overwriting/colliding with already used PilVars?
    --       This could happen for synthesized PilVars with a Nothing context.
  , definedVars :: [PilVar]
    -- | All PilVars referenced for all contexts.
    -- This differs from _definedVars, as order is not preserved and referenced,
    -- but undefined, PilVars are included
  , usedVars :: HashSet PilVar
    -- TODO: This is fixed to BN MLIL SSA variables here, but will be generalized
    --       when moving to a PilImporter instance.
    -- TODO: Does this need to be a set or just a single variable?
    -- | A mapping of PilVars to the a variable from the import source.
  , sourceVars :: HashMap PilVar SSAVariableRef
    -- | Map of known functions with parameter access information
  , knownFuncs :: HashMap Text FuncInfo
    -- | Address size based on target platform
  , addrSize :: AddressWidth
    -- | Default variable size, usually based on platform default
  , defaultVarSize :: Bits
  , binaryView :: Binja.BNBinaryView
  }
  deriving (Eq, Show, Generic)


data SSAVariableRef = SSAVariableRef
  { var :: Mlil.SSAVariable
  , func :: BNFunc.Function
  , ctxIndex :: Pil.CtxIndex
  }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)


type F = BNFunc.MLILSSAFunction
