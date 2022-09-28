module Blaze.Import.Source.Ghidra.Pil where

import Blaze.Prelude hiding (Symbol)

import qualified Ghidra.BasicBlock as BB
import Ghidra.State (GhidraState)
import qualified Ghidra.Core as Ghidra
import Ghidra.Types.Function (Function)
import qualified Ghidra.Function as GFunc
import qualified Ghidra.Pcode as Pcode
import qualified Ghidra.Types.Pcode as Pcode
import qualified Ghidra.State as GState
import Ghidra.Types.Pcode.Lifted (PcodeOp)
import qualified Ghidra.Types.Pcode.Lifted as P

import qualified Blaze.Pil as Pil
import qualified Data.BinaryAnalysis as BA
import qualified Prelude as P
import Blaze.Types.Pil
    ( BranchCondOp(BranchCondOp),
      CallOp(CallOp),
      TailCallOp(TailCallOp),
      Ctx(Ctx),
      DefOp(DefOp),
      DefPhiOp(DefPhiOp),
      Expression(Expression),
      PilVar(PilVar),
      RetOp(RetOp),
      Statement(BranchCond, Call, Def, DefPhi, Nop, Ret, Store, Undef,
                UnimplInstr, UnimplMem, NoRet, TailCall, Jump, JumpTo),
      Stmt,
      StoreOp(StoreOp),
      Symbol,
      UnimplMemOp(UnimplMemOp),
      Ctx,
      CtxId,
      Expression,
      PilVar, JumpOp (JumpOp), JumpToOp (JumpToOp) )

import Ghidra.Types.Variable (HighVarNode, VarNode, VarType)
import qualified Blaze.Import.Source.Ghidra.CallGraph as GCG
import Blaze.Import.Source.BinaryNinja.Types
import qualified Blaze.Types.Function as Func
import qualified Blaze.Types.Pil as Pil
import Blaze.Util.GenericConv (GConv, gconv)
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.Text as Text


data ConverterState = ConverterState
  deriving (Eq, Ord, Show, Generic)

-- TODO: Add map of PilVars to original vars to the state being tracked
newtype Converter a = Converter { _runConverter :: StateT ConverterState IO a}
  deriving (Functor)
  deriving newtype (Applicative, Monad, MonadState ConverterState, MonadIO)


class IsVariable a where
  getSize :: a -> Bytes
  getVarType :: a -> VarType

instance IsVariable HighVarNode where
  getSize = view #size
  getVarType = view #varType

instance IsVariable VarNode where
  getSize = view #size
  getVarType = view #varType

convertPcodeOpToPilStmt :: IsVariable a => PcodeOp a -> Converter Pil.Stmt
convertPcodeOpToPilStmt op = case op of
  P.BOOL_AND out in1 in2 -> undefined
  P.BOOL_NEGATE out in1 -> undefined
  P.BOOL_OR out in1 in2 -> undefined
  P.BOOL_XOR out in1 in2 -> undefined
  P.BRANCH dest -> undefined
  P.BRANCHIND in1 -> undefined
  P.CALL dest inputs -> undefined
  P.CALLIND in1 inputs -> undefined
  P.CALLOTHER in1 inputs -> undefined -- Can't find this in the docs
  P.CAST out in1 -> undefined
  P.CBRANCH dest in1 -> undefined
  P.COPY out in1 -> undefined
  P.CPOOLREF out in1 in2 inputs -> undefined
  P.EXTRACT out in1 in2 -> undefined -- NOT in docs. guessing
  P.FLOAT_ABS out in2 -> undefined
  P.FLOAT_ADD out in1 in2 -> undefined
  P.FLOAT_CEIL out in1 -> undefined
  P.FLOAT_DIV out in1 in2 -> undefined
  P.FLOAT_EQUAL out in1 in2 -> undefined
  P.FLOAT_FLOAT2FLOAT out in1 -> undefined
  P.FLOAT_FLOOR out in1 -> undefined
  P.FLOAT_INT2FLOAT out in1 -> undefined
  P.FLOAT_LESS out in1 in2 -> undefined
  P.FLOAT_LESSEQUAL out in1 in2 -> undefined
  P.FLOAT_MULT out in1 in2 -> undefined
  P.FLOAT_NAN out in1 -> undefined
  P.FLOAT_NEG out in1 -> undefined
  P.FLOAT_NOTEQUAL out in1 in2 -> undefined
  P.FLOAT_ROUND out in1 -> undefined
  P.FLOAT_SQRT out in1 -> undefined
  P.FLOAT_SUB out in1 in2 -> undefined
  P.FLOAT_TRUNC out in1 in2 -> undefined -- not in docs
  P.INDIRECT out in1 in2 -> undefined
  P.INSERT -> undefined -- not in docs
  P.INT_2COMP out in1 -> undefined
  P.INT_ADD out in1 in2 -> undefined
  P.INT_AND out in1 in2 -> undefined
  P.INT_CARRY out in1 in2 -> undefined
  P.INT_DIV out in1 in2 -> undefined
  P.INT_EQUAL out in1 in2 -> undefined
  P.INT_LEFT out in1 in2 -> undefined
  P.INT_LESS out in1 in2 -> undefined
  P.INT_LESSEQUAL out in1 in2 -> undefined
  P.INT_MULT out in1 in2 -> undefined
  P.INT_NEGATE out in1 -> undefined
  P.INT_NOTEQUAL out in1 in2 -> undefined
  P.INT_OR out in1 in2 -> undefined
  P.INT_REM out in1 in2 -> undefined
  P.INT_RIGHT out in1 in2 -> undefined
  P.INT_SBORROW out in1 in2 -> undefined
  P.INT_SCARRY out in1 in2 -> undefined
  P.INT_SDIV out in1 in2 -> undefined
  P.INT_SEXT out in1 -> undefined
  P.INT_SLESS out in1 in2 -> undefined
  P.INT_SLESSEQUAL out in1 in2 -> undefined
  P.INT_SREM out in1 in2 -> undefined
  P.INT_SRIGHT out in1 in2 -> undefined
  P.INT_SUB out in1 in2 -> undefined
  P.INT_XOR out in1 in2 -> undefined
  P.INT_ZEXT out in1 -> undefined
  P.LOAD out addrSpace in2 -> undefined
  P.MULTIEQUAL out in1 in2 inputs -> undefined
  P.NEW out in1 inputs -> undefined
  P.PCODE_MAX -> undefined -- unknown
  P.PIECE out in1 in2 -> undefined
  P.POPCOUNT out in1 -> undefined
  P.PTRADD out in1 in2 in3 -> undefined
  P.PTRSUB out in1 in2 -> undefined
  P.RETURN in1 inputs -> undefined
  P.SEGMENTOP -> undefined -- unknowng
  P.STORE addrSpace in1 in2 -> undefined
  P.SUBPIECE out off in1 -> undefined
  P.UNIMPLEMENTED -> undefined

