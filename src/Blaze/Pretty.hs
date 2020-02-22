module Blaze.Pretty
  ( Pretty( pretty )
  , prettyPrint
  , pp
  , PStmts (PStmts)
  ) where

import Protolude hiding (Symbol, sym, const)

import Text.Printf
import qualified Numeric
import qualified Data.Set as Set
import qualified Data.Map as Map
import Binja.Core (InstructionIndex(InstructionIndex), Address(Address))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Coerce (coerce)

import qualified Binja.MLIL as MLIL
import qualified Binja.Function
import qualified Binja.Variable

import qualified Blaze.Types.Pil as Pil
import Blaze.Pil.Display (Symbol)

import Control.Lens hiding (op)

-- TODO: make pretty return a monad instead of text,
-- which can do things like `indent`

class Pretty x where
  pretty :: x -> Text

showHex :: (Integral a, Show a) => a -> Text
showHex x = Text.pack $ "0x" <> Numeric.showHex x ""

instance Pretty Int where
  pretty = show

instance Pretty (InstructionIndex a) where
  pretty (InstructionIndex x) = show x

instance Pretty Address where
  pretty (Address x) = showHex x

instance Pretty a => Pretty [a] where
  pretty ys = "[" <> f ys <> "]" where
    f [] = ""
    f [x] = pretty x
    f (x:xs) = pretty x <> ", " <> f xs

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty m = "Map: " <> pretty (Map.toList m)

instance Pretty a => Pretty (Set a) where
  pretty ys = "#{" <> f (Set.toList ys) <> "}" where
    f [] = ""
    f [x] = pretty x
    f (x:xs) = pretty x <> ", " <> f xs

instance Pretty (MLIL.Expression a) where
  pretty _ = "(TODO: MLIL Expression)"

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = "(" <> pretty a <> ", " <> pretty b <> ")"

---- PIL

instance Pretty Binja.Variable.Variable where
  pretty v = v ^. Binja.Variable.name

instance Pretty MLIL.SSAVariable where
  pretty ssaVar = Text.pack $ printf "%s@%s" name version
    where
      name :: Text
      name = pretty (ssaVar ^. MLIL.var)
      version :: Text
      version = show (ssaVar ^. MLIL.version)

instance Pretty Pil.PilVar where
  pretty v = v ^. Pil.symbol

instance Pretty Pil.OperationSize where
  pretty (Pil.OperationSize sz) = show sz

instance Pretty (Pil.CallDest Pil.Expression) where
  pretty dest = case dest of
    (Pil.CallConstPtr ptr) -> show (ptr ^. Pil.constant)
    (Pil.CallExpr e) -> pretty e
    (Pil.CallExprs es) -> show $ fmap pretty es

instance Pretty Binja.Function.Function where
  pretty f = Text.pack $ printf "%s @0x%x" name start
    where
      name = f ^. Binja.Function.name
      start :: Word64
      start = coerce $ f ^. Binja.Function.start

instance Pretty Pil.SimpleCtx where
  pretty ctx = Text.pack $ printf "simpCtx (%s) %s" func idx
    where
      func :: Text
      func = maybe "Nothing" pretty (ctx ^. Pil.func)
      idx :: Text
      idx = maybe "Nothing" show (ctx ^. Pil.ctxIndex)


prettyBinop ::
  ( Pil.HasLeft a Pil.Expression,
    Pil.HasRight a Pil.Expression
  ) =>
  Symbol ->
  a ->
  Text
prettyBinop sym op = Text.pack $ printf "(%s) %s (%s)" left sym right
  where
    left = pretty (op ^. Pil.left)
    right = pretty (op ^. Pil.right)

prettyUnop ::
  ( Pil.HasSrc a b,
    Pretty b
  ) =>
  Symbol ->
  a ->
  Text
prettyUnop sym op = Text.pack $ printf "%s(%s)" sym src
  where
    src = pretty (op ^. Pil.src)

prettyConst ::
  ( Pil.HasConstant a b,
    Show b
  ) =>
  a ->
  Text
prettyConst op = Text.pack $ printf "%s" const
  where
    const :: Text
    const = show (op ^. Pil.constant)

prettyVar :: (Pil.HasSrc a Pil.PilVar) => a -> Text
prettyVar op = Text.pack $ printf "%s" src
  where
    src = pretty (op ^. Pil.src)

prettyField ::
  ( Pil.HasSrc a b,
    Pretty b,
    Pil.HasOffset a Int64
  ) =>
  a ->
  Text
prettyField op = Text.pack $ printf "%s[%s]" src offset
  where
    src = pretty (op ^. Pil.src)
    offset :: Text
    offset = show (op ^. Pil.offset)

instance Pretty Pil.Expression where
  pretty (Pil.Expression _ exprOp) = case exprOp of
    (Pil.ADC op) -> prettyBinop "adc" op
    (Pil.ADD op) -> prettyBinop "add" op
    (Pil.ADDRESS_OF op) -> prettyUnop "&" op
    (Pil.ADDRESS_OF_FIELD op) -> Text.pack $ printf "&(%s)" (Text.unpack $ prettyField op)
    (Pil.ADD_OVERFLOW op) -> prettyBinop "addOf" op
    (Pil.AND op) -> prettyBinop "and" op
    (Pil.ASR op) -> prettyBinop "asr" op
    (Pil.BOOL_TO_INT op) -> prettyUnop "boolToInt" op
    (Pil.CEIL op) -> prettyUnop "ceil" op
    (Pil.CMP_E op) -> prettyBinop "cmpE" op
    (Pil.CMP_NE op) -> prettyBinop "cmpNE" op
    (Pil.CMP_SGE op) -> prettyBinop "cmpSGE" op
    (Pil.CMP_SGT op) -> prettyBinop "cmpSGT" op
    (Pil.CMP_SLE op) -> prettyBinop "cmpSLE" op
    (Pil.CMP_SLT op) -> prettyBinop "cmpSLT" op
    (Pil.CMP_UGE op) -> prettyBinop "cmpUGE" op
    (Pil.CMP_UGT op) -> prettyBinop "cmpUGT" op
    (Pil.CMP_ULE op) -> prettyBinop "cmpULE" op
    (Pil.CMP_ULT op) -> prettyBinop "cmpULT" op
    (Pil.CONST op) -> prettyConst op
    (Pil.CONST_PTR op) -> prettyConst op
    (Pil.DIVS op) -> prettyBinop "divs" op
    (Pil.DIVS_DP op) -> prettyBinop "divsDP" op
    (Pil.DIVU op) -> prettyBinop "divu" op
    (Pil.DIVU_DP op) -> prettyBinop "divuDP" op
    (Pil.FABS op) -> prettyUnop "fabs" op
    (Pil.FADD op) -> prettyBinop "fadd" op
    (Pil.FCMP_E op) -> prettyBinop "fcmpE" op
    (Pil.FCMP_GE op) -> prettyBinop "fcmpGE" op
    (Pil.FCMP_GT op) -> prettyBinop "fcmpGT" op
    (Pil.FCMP_LE op) -> prettyBinop "fcmpLE" op
    (Pil.FCMP_LT op) -> prettyBinop "fcmpLT" op
    (Pil.FCMP_NE op) -> prettyBinop "fcmpNE" op
    (Pil.FCMP_O op) -> prettyBinop "fcmpO" op
    (Pil.FCMP_UO op) -> prettyBinop "fcmpUO" op
    (Pil.FDIV op) -> prettyBinop "fdiv" op
    (Pil.FLOAT_CONST op) -> prettyConst op
    (Pil.FLOAT_CONV op) -> prettyUnop "floatConv" op
    (Pil.FLOAT_TO_INT op) -> prettyUnop "floatToInt" op
    (Pil.FLOOR op) -> prettyUnop "floor" op
    (Pil.FMUL op) -> prettyBinop "fmul" op
    (Pil.FNEG op) -> prettyUnop "fneg" op
    (Pil.FSQRT op) -> prettyUnop "fsqrt" op
    (Pil.FSUB op) -> prettyBinop "fsub" op
    (Pil.FTRUNC op) -> prettyUnop "ftrunc" op
    (Pil.IMPORT op) -> prettyConst op
    (Pil.INT_TO_FLOAT op) -> prettyUnop "intToFloat" op
    (Pil.LOAD op) -> prettyUnop "load" op
    -- TODO: add memory versions for all SSA ops
    (Pil.LOW_PART op) -> prettyUnop "lowPart" op
    (Pil.LSL op) -> prettyBinop "lsl" op
    (Pil.LSR op) -> prettyBinop "lsr" op
    (Pil.MODS op) -> prettyBinop "mods" op
    (Pil.MODS_DP op) -> prettyBinop "modsDP" op
    (Pil.MODU op) -> prettyBinop "modu" op
    (Pil.MODU_DP op) -> prettyBinop "moduDP" op
    (Pil.MUL op) -> prettyBinop "mul" op
    (Pil.MULS_DP op) -> prettyBinop "mulsDP" op
    (Pil.MULU_DP op) -> prettyBinop "muluDP" op
    (Pil.NEG op) -> prettyUnop "neg" op
    (Pil.NOT op) -> prettyUnop "not" op
    (Pil.OR op) -> prettyBinop "or" op
    -- TODO: Need to add carry
    (Pil.RLC op) -> prettyBinop "rlc" op
    (Pil.ROL op) -> prettyBinop "rol" op
    (Pil.ROR op) -> prettyBinop "ror" op
    (Pil.ROUND_TO_INT op) -> prettyUnop "roundToInt" op
    -- TODO: Need to add carry
    (Pil.RRC op) -> prettyBinop "rrc" op
    (Pil.SBB op) -> prettyBinop "sbb" op
    (Pil.SUB op) -> prettyBinop "sub" op
    (Pil.SX op) -> prettyUnop "sx" op
    (Pil.TEST_BIT op) -> prettyBinop "testBit" op
    (Pil.UNIMPL t) -> "unimpl (" <> t <> ")"
    (Pil.VAR_ALIASED op) -> prettyVar op
    -- TODO: Add field offset
    (Pil.VAR_ALIASED_FIELD op) -> prettyField op
    (Pil.VAR_PHI op) -> Text.pack $ printf "%s <- %s" (pretty (op ^. Pil.dest)) srcs
      where 
        srcs :: Text
        srcs = show (fmap pretty (op ^. Pil.src))
    (Pil.VAR_SPLIT op) -> Text.pack $ printf "varSplit %s %s" (pretty (op ^. Pil.high)) (pretty (op ^. Pil.low))
    -- (Pil.VAR op) -> Text.pack $ printf "var \"%s\" %s" (pretty $ op ^. Pil.src) (pretty)
    -- TODO: Need added
    (Pil.VAR op) -> prettyVar op
    -- TODO: Add field offset
    (Pil.VAR_FIELD op) -> prettyField op
    (Pil.XOR op) -> prettyBinop "xor" op
    (Pil.ZX op) -> prettyUnop "zx" op
    (Pil.CALL op) -> case op ^. Pil.name of
      (Just name) -> Text.pack $ printf "call %s %s %s" name dest params
      Nothing -> Text.pack $ printf "call (Nothing) %s %s" dest params
      where 
        dest = pretty (op ^. Pil.dest)
        params :: Text
        params = show (fmap pretty (op ^. Pil.params))
    (Pil.StrCmp op) -> prettyBinop "strcmp" op
    (Pil.StrNCmp op) -> Text.pack $ printf "strncmp %d %s %s" (op ^. Pil.len) (pretty (op ^. Pil.left)) (pretty (op ^. Pil.right))
    (Pil.MemCmp op) -> prettyBinop "memcmp" op
    -- TODO: Should ConstStr also use const rather than value as field name?
    (Pil.ConstStr op) -> Text.pack $ printf "constStr \"%s\"" $ op ^. Pil.value
    (Pil.Extract op) -> Text.pack $ printf "extract %s %d" (pretty (op ^. Pil.src)) (op ^. Pil.offset)

instance Pretty Pil.Stmt where
  pretty stmt = case stmt of
    (Pil.Def x) -> Text.pack $ printf "%s = %s" (pretty $ x ^. Pil.var) (pretty $ x ^. Pil.value)
    (Pil.Constraint x) -> Text.pack $ printf "?: %s" (pretty $ x ^. Pil.condition)
    (Pil.Store x) -> Text.pack $ printf "[%s] = %s" (pretty $ x ^. Pil.addr) (pretty $ x ^. Pil.value)
    Pil.UnimplInstr -> "Unimplemented Instruction"
    (Pil.UnimplMem x) -> Text.pack $ printf "Unimplemented Memory: [%s]" (pretty $ x ^. Pil.src)
    Pil.Undef -> "Undefined"
    Pil.Nop -> "Nop"
    (Pil.Annotation t) -> "Annotation: " <> t
    (Pil.EnterContext x) -> "----> Entering " <> pretty (x ^. Pil.ctx)
    (Pil.ExitContext x) -> "<---- Leaving %s" <> pretty (x ^. Pil.leavingCtx)

newtype PStmts = PStmts [Pil.Stmt]

instance Pretty PStmts where
  pretty (PStmts stmts) = Text.intercalate "\n" . fmap pretty $ stmts

-- |Pretty print to IO.
prettyPrint :: (MonadIO m, Pretty a) => a -> m ()
prettyPrint = putText . pretty

pp :: (MonadIO m, Pretty a) => a -> m ()
pp = prettyPrint
