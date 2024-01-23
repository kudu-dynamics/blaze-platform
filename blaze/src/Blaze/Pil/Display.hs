{-# LANGUAGE UndecidableInstances #-}
module Blaze.Pil.Display where

import qualified Binja.Function
import qualified Binja.MLIL
import qualified Data.HashMap.Strict as HMap
import qualified Binja.Variable
import Blaze.Prelude hiding (Symbol, const, sym, bracket)
import Blaze.Types.Function (Function)
import Blaze.Types.Pil (Size)
import qualified Blaze.Types.Pil as Pil
import qualified Data.Text as Text
import Text.Printf
import Blaze.Types.Pil.Checker (InfoExpression)

type Symbol = Text

class NeedsParens a where
  -- | True if subterm is complex enough to need parentheses when part of a
  -- larger term
  needsParens :: a -> Bool

instance NeedsParens (Pil.ExprOp a) where
  needsParens :: Pil.ExprOp a -> Bool
  needsParens = \case
    Pil.CONST _ -> False
    Pil.CONST_PTR _ -> False
    Pil.CONST_FLOAT _ -> False
    Pil.LOAD _ -> False
    Pil.STACK_LOCAL_ADDR _ -> False
    Pil.VAR _ -> False
    Pil.VAR_FIELD _ -> False
    Pil.ConstStr _ -> False
    Pil.ConstFuncPtr _ -> False
    _ -> True

instance NeedsParens Pil.Expression where
  needsParens :: Pil.Expression -> Bool
  needsParens = needsParens . view #op

instance NeedsParens (InfoExpression a) where
  needsParens :: InfoExpression a -> Bool
  needsParens = needsParens . view #op

---- Helpers for displaying common patterns of expressions
dispBinop
  :: ( Disp b
     , HasField' "left" a b
     , HasField' "right" a b
     )
  => Symbol
  -> a
  -> Size c
  -> Text
dispBinop sym op sz = Text.pack $ printf "%s (%s) (%s) %s" sym left right $ disp sz
  where
    left = disp (op ^. #left)
    right = disp (op ^. #right)

dispUnop
  :: ( HasField' "src" a b
     , Disp b
     )
  => Symbol
  -> a
  -> Size c
  -> Text
dispUnop sym op sz = Text.pack $ printf "%s (%s) %s" sym src $ disp sz
  where
    src = disp (op ^. #src)

dispConst
  :: ( HasField' "constant" a b
     , Show b
     )
  => Symbol
  -> a
  -> Size c
  -> Text
dispConst sym op sz = Text.pack $ printf "%s %s %s" sym const (disp sz)
  where
    const :: Text
    const = show (op ^. #constant)

dispField
  :: ( HasField' "src" a b
     , Disp b
     , HasField' "offset" a ByteOffset
     )
  => Symbol
  -> a
  -> Size c
  -> Text
dispField sym op sz = Text.pack $ printf "%s %s[%s] %s" sym src offset $ disp sz
  where
    src = disp (op ^. #src)
    offset :: Text
    offset = show (op ^. #offset)

dispVar :: (HasField' "src" a Pil.PilVar) => Symbol -> a -> Size b -> Text
dispVar sym op sz = Text.pack $ printf "%s \"%s\" %s" sym src $ disp sz
  where
    src = disp (op ^. #src)

parenExpr :: (Disp a, NeedsParens a) => a -> Text
parenExpr x = if needsParens x
  then paren (disp x)
  else disp x

paren :: Text -> Text
paren t = "(" <> t <> ")"

bracket :: Text -> Text
bracket t = "[" <> t <> "]"

curly :: Text -> Text
curly t = "{" <> t <> "}"

commas :: [Text] -> Text
commas = Text.intercalate ", "

asList :: [Text] -> Text
asList = bracket . commas

asTuple :: [Text] -> Text
asTuple = paren . commas

asCurlyList :: [Text] -> Text
asCurlyList = curly . commas

asMultilineList :: [Text] -> Text
asMultilineList = bracket . Text.cons ' ' . Text.intercalate "\n, "

(<->) :: Text -> Text -> Text
(<->) a b = a <> " " <> b

class Disp a where
  disp :: a -> Text

instance Disp Binja.Variable.Variable where
  disp v = v ^. Binja.Variable.name

instance Disp Binja.MLIL.SSAVariable where
  disp ssaVar = Text.pack $ printf "%s@%s" name version
    where
      name :: Text
      name = disp (ssaVar ^. Binja.MLIL.var)
      version :: Text
      version = show (ssaVar ^. Binja.MLIL.version)

instance Disp Address where
  disp = show

instance Disp Int64 where
  disp = show

instance Disp Pil.PilVar where
  disp v = v ^. #symbol

instance Disp (Pil.Size a) where
  disp (Pil.Size (Bytes sz)) = show sz

instance Disp a => Disp (Pil.CallDest a) where
  disp dest = case dest of
    Pil.CallAddr addr -> show addr
    Pil.CallExpr e -> disp e
    Pil.CallExprs es -> show $ fmap disp es
    Pil.CallFunc fn -> disp fn
    Pil.CallExtern x -> disp x
    Pil.CallUnk -> "unknown call dest"

instance Disp Pil.ExternPtrOp where
  disp x = Text.pack $ printf "extern (%s) (%s) (%s)"
           (show $ x ^. #address :: String)
           (disp $ x ^. #offset)
           (show $ x ^. #symbol :: String)

instance Disp Binja.Function.Function where
  disp f = Text.pack $ printf "func \"%s\" %s" name start
    where
      name = f ^. Binja.Function.name
      start :: Text
      start = show $ f ^. Binja.Function.start

instance Disp Pil.Ctx where
  disp ctx = Text.pack $ printf "simpCtx (%s) %s" func idx
    where
      func :: Text
      func = disp $ ctx ^. #func
      idx :: Text
      idx = show $ ctx ^. #ctxId

instance (Disp a, NeedsParens a) => Disp (Pil.Statement a) where
  disp stmt = case stmt of
    Pil.BranchCond op -> Text.pack $ printf "branch (%s)" cond
      where
        cond = disp $ op ^. #cond
    Pil.Jump op -> Text.pack $ printf "jump (%s)" dest
      where
        dest = disp $ op ^. #dest
    Pil.JumpTo op -> Text.pack $ printf "jump_to (%s) %s" dest targets
      where
        dest = disp $ op ^. #dest
        targets = asList $ disp <$> op ^. #targets
    (Pil.Def op) -> Text.pack $ printf "def \"%s\" (%s)" var val
      where
        var = disp $ op ^. #var
        val = disp $ op ^. #value
    (Pil.Constraint op) -> disp $ op ^. #condition
    (Pil.Store op) -> Text.pack $ printf "store (%s) (%s)" addr val
      where
        addr = disp $ op ^. #addr
        val = disp $ op ^. #value
    Pil.UnimplInstr t -> "unimplInstr (\"" <> t <> "...\")"
    (Pil.UnimplMem op) -> Text.pack $ printf "unimplMem (%s)" src
      where
        src = disp $ op ^. #src
    Pil.Undef -> "undef"
    Pil.Nop -> "nop"
    (Pil.Annotation ann) -> Text.pack $ printf "ann %s" (show ann :: String)
    (Pil.EnterContext op) -> Text.pack $ printf "enter (%s)" ctx
      where
        ctx = disp $ op ^. #ctx
    (Pil.ExitContext op) -> Text.pack $ printf "exit (%s) (%s)" exitCtx retCtx
      where
        exitCtx = disp $ op ^. #leavingCtx
        retCtx = disp $ op ^. #returningToCtx
    (Pil.Call c) -> case c ^. #name of
      (Just name) -> Text.pack $ printf "call (%s) %s" name args
      Nothing -> Text.pack $ printf "call (Nothing) (%s) %s" dest args
      where
        dest = disp $ c ^. #dest
        args :: Text
        args = show $ fmap disp $ c ^. #args
    (Pil.DefPhi op) -> Text.pack $ printf "defPhi \"%s\" %s" var val
      where
        var = disp $ op ^. #dest
        val = asList . fmap disp $ op ^. #src
    (Pil.Ret op) -> "Ret" <-> parenExpr (op ^. #value)
    (Pil.DefMemPhi op) -> Text.pack $ printf "defMemPhi \"%s\" %s" var val
      where
        showMem :: Int64 -> Text
        showMem n = "mem#" <> show n
        var = showMem $ op ^. #destMemory
        val = asList . fmap showMem $ op ^. #srcMemory
    Pil.NoRet -> "NoRet"
    Pil.Exit -> "Exit"
    (Pil.TailCall op) -> case op ^. #name of
      (Just name) -> Text.pack $ printf "Tailcall (%s) %s" name args
      Nothing -> Text.pack $ printf "Tailcall (Nothing) (%s) %s" dest args
      where
        dest = disp $ op ^. #dest
        args :: Text
        args = show $ fmap disp $ op ^. #args

dispExprOp :: (Disp a, NeedsParens a)
           => Pil.ExprOp a
           -> Pil.Size a
           -> Text
dispExprOp exprOp size = case exprOp of
  (Pil.ADC op) -> dispBinop "adc" op size
  (Pil.ADD op) -> dispBinop "add" op size
  (Pil.ADD_WILL_CARRY op) -> dispBinop "addWillCarry" op size
  (Pil.ADD_WILL_OVERFLOW op) -> dispBinop "addWillOverflow" op size
  (Pil.AND op) -> dispBinop "and" op size
  (Pil.ARRAY_ADDR op) ->
    "arrayAddr"
    <-> parenExpr (op ^. #base)
    <-> parenExpr (op ^. #index)
    <-> paren (show $ op ^. #stride)
  (Pil.ASR op) -> dispBinop "asr" op size
  (Pil.BOOL_TO_INT op) -> dispUnop "boolToInt" op size
  (Pil.CEIL op) -> dispUnop "ceil" op size
  (Pil.CMP_E op) -> dispBinop "cmpE" op size
  (Pil.CMP_NE op) -> dispBinop "cmpNE" op size
  (Pil.CMP_SGE op) -> dispBinop "cmpSGE" op size
  (Pil.CMP_SGT op) -> dispBinop "cmpSGT" op size
  (Pil.CMP_SLE op) -> dispBinop "cmpSLE" op size
  (Pil.CMP_SLT op) -> dispBinop "cmpSLT" op size
  (Pil.CMP_UGE op) -> dispBinop "cmpUGE" op size
  (Pil.CMP_UGT op) -> dispBinop "cmpUGT" op size
  (Pil.CMP_ULE op) -> dispBinop "cmpULE" op size
  (Pil.CMP_ULT op) -> dispBinop "cmpULT" op size
  (Pil.CONST op) -> dispConst "const" op size
  (Pil.CONST_BOOL op) -> "constBool" <-> show (op ^. #constant) <-> disp size
  (Pil.CONST_FLOAT op) -> dispConst "float" op size
  (Pil.CONST_PTR op) -> dispConst "constPtr" op size
  (Pil.DIVS op) -> dispBinop "divs" op size
  (Pil.DIVS_DP op) -> dispBinop "divsDP" op size
  (Pil.DIVU op) -> dispBinop "divu" op size
  (Pil.DIVU_DP op) -> dispBinop "divuDP" op size
  (Pil.FABS op) -> dispUnop "fabs" op size
  (Pil.FADD op) -> dispBinop "fadd" op size
  (Pil.FCMP_E op) -> dispBinop "fcmpE" op size
  (Pil.FCMP_GE op) -> dispBinop "fcmpGE" op size
  (Pil.FCMP_GT op) -> dispBinop "fcmpGT" op size
  (Pil.FCMP_LE op) -> dispBinop "fcmpLE" op size
  (Pil.FCMP_LT op) -> dispBinop "fcmpLT" op size
  (Pil.FCMP_NE op) -> dispBinop "fcmpNE" op size
  (Pil.FCMP_O op) -> dispBinop "fcmpO" op size
  (Pil.FCMP_UO op) -> dispBinop "fcmpUO" op size
  (Pil.FDIV op) -> dispBinop "fdiv" op size
  (Pil.FIELD_ADDR op) ->
    "fieldAddr"
    <-> parenExpr (op ^. #baseAddr)
    <-> disp (op ^. #offset)
  (Pil.FLOAT_CONV op) -> dispUnop "floatConv" op size
  (Pil.FLOAT_TO_INT op) -> dispUnop "floatToInt" op size
  (Pil.FLOOR op) -> dispUnop "floor" op size
  (Pil.FMUL op) -> dispBinop "fmul" op size
  (Pil.FNEG op) -> dispUnop "fneg" op size
  (Pil.FSQRT op) -> dispUnop "fsqrt" op size
  (Pil.FSUB op) -> dispBinop "fsub" op size
  (Pil.FTRUNC op) -> dispUnop "ftrunc" op size
  (Pil.IMPORT op) -> dispConst "import" op size
  (Pil.INT_TO_FLOAT op) -> dispUnop "intToFloat" op size
  (Pil.LOAD op) -> dispUnop "load" op size
  -- TODO: add memory versions for all SSA ops
  (Pil.LOW_PART op) -> dispUnop "lowPart" op size
  (Pil.LSL op) -> dispBinop "lsl" op size
  (Pil.LSR op) -> dispBinop "lsr" op size
  (Pil.MODS op) -> dispBinop "mods" op size
  (Pil.MODS_DP op) -> dispBinop "modsDP" op size
  (Pil.MODU op) -> dispBinop "modu" op size
  (Pil.MODU_DP op) -> dispBinop "moduDP" op size
  (Pil.MUL op) -> dispBinop "mul" op size
  (Pil.MULS_DP op) -> dispBinop "mulsDP" op size
  (Pil.MULU_DP op) -> dispBinop "muluDP" op size
  (Pil.NEG op) -> dispUnop "neg" op size
  (Pil.NOT op) -> dispUnop "not" op size
  (Pil.OR op) -> dispBinop "or" op size
  (Pil.POPCNT op) -> dispUnop "popcnt" op size
  -- TODO: Need to add carry
  (Pil.RLC op) -> dispBinop "rlc" op size
  (Pil.ROL op) -> dispBinop "rol" op size
  (Pil.ROR op) -> dispBinop "ror" op size
  (Pil.ROUND_TO_INT op) -> dispUnop "roundToInt" op size
  -- TODO: Need to add carry
  (Pil.RRC op) -> dispBinop "rrc" op size
  (Pil.SBB op) -> dispBinop "sbb" op size
  (Pil.STACK_LOCAL_ADDR op) -> "stackLocalAddr" <-> paren (disp $ op ^. #stackOffset)
  (Pil.SUB op) -> dispBinop "sub" op size
  (Pil.SUB_WILL_OVERFLOW op) -> dispBinop "subWillOverflow" op size
  (Pil.SX op) -> dispUnop "sx" op size
  (Pil.TEST_BIT op) -> dispBinop "testBit" op size
  (Pil.UNIMPL t) -> "unimpl" <-> paren t
  (Pil.UPDATE_VAR op) ->
    "updateVar"
    <-> disp (op ^. #dest)
    <-> disp (op ^. #offset)
    <-> parenExpr (op ^. #src)
  (Pil.VAR_JOIN op) -> Text.pack $ printf "varJoin %s %s %s" (disp (op ^. #high)) (disp (op ^. #low)) (disp size)
  -- (Pil.VAR op) -> Text.pack $ printf "var \"%s\" %s" (disp $ op ^. Pil.src) (disp size)
  -- TODO: Need size added
  (Pil.VAR op) -> dispVar "var" op size
  -- TODO: Add field offset
  (Pil.VAR_FIELD op) -> dispField "varField" op size
  (Pil.XOR op) -> dispBinop "xor" op size
  (Pil.ZX op) -> dispUnop "zx" op size
  (Pil.CALL op) -> case op ^. #name of
    (Just name) -> Text.pack $ printf "call %s %s %s" name dest args
    Nothing -> Text.pack $ printf "call (Nothing) %s %s" dest args
    where
      dest = disp (op ^. #dest)
      args :: Text
      args = show (fmap disp (op ^. #args))
  (Pil.StrCmp op) -> dispBinop "strcmp" op size
  (Pil.StrNCmp op) -> Text.pack $ printf "strncmp %d %s %s %s" (op ^. #len) (disp (op ^. #left)) (disp (op ^. #right)) (disp size)
  (Pil.MemCmp op) -> dispBinop "memcmp" op size
  (Pil.ExternPtr op) -> disp op
  -- TODO: Should ConstStr also use const rather than value as field name?
  (Pil.ConstStr op) -> Text.pack $ printf "constStr \"%s\"" $ op ^. #value
  (Pil.ConstFuncPtr op) -> Text.pack $ printf "constFuncPtr %s %s"
                           (maybe "Nothing" (("Just" <>) . cs) $ op ^. #symbol :: Text)
                           (show $ op ^. #address :: Text)
  (Pil.Extract op) -> "extract " <> disp (op ^. #src) <> disp (op ^. #offset)
  Pil.UNIT -> "UNIT"

instance Disp Pil.Expression where
  disp (Pil.Expression size exprOp) = dispExprOp exprOp size

-- TODO: Replace existing instances with these or remove them
-- instance Disp Pil.SimpleCtx where
--   disp ctx = "< " <> fname <> i <> " >"
--     where
--       fname = maybe "<Unknown Function>" identity
--               . fmap (view Func.name) $ ctx ^. Pil.func
--       i = maybe "" (("#" <>) . show) $ ctx ^. Pil.ctxId

instance Disp Pil.StackOffset where
  disp x =
    "stackOffset"
    <-> show (x ^. #offset)
    <-> paren (disp (x ^. #ctx))


instance Disp Function where
  disp f = Text.pack $ printf "func \"%s\" %s" name start
    where
      name = f ^. #name
      start :: Text
      start = show $ f ^. #address

instance Disp ByteOffset where
  disp (ByteOffset x) = "byteOffset " <> show x

instance Disp Bytes where
  disp (Bytes x) = "bytes " <> show x

instance Disp [Pil.Stmt] where
  disp = Text.intercalate "\n" . fmap disp

instance (Disp a, Disp b) => Disp (a, b) where
  disp (a, b) = "(" <> disp a <> ", " <> disp b <> ")"

instance (Disp a, Disp b) => Disp (HashMap a b) where
  disp = Text.intercalate "\n" . fmap disp . HMap.toList

pdisp :: (MonadIO m, Disp a) => a -> m ()
pdisp = putText . disp
