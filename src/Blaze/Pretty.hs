module Blaze.Pretty
  ( Pretty (pretty),
    prettyPrint,
    pp,
    PStmts (PStmts),
    prettyStmts,
    prettyIndexedStmts,
    showHex,
  )
where

import Blaze.Prelude hiding (Symbol, const, sym, bracket)
import Binja.Core (InstructionIndex (InstructionIndex))
import qualified Binja.Function
import qualified Binja.MLIL as MLIL
import qualified Binja.Variable
import Blaze.Pil.Display ((<->), Symbol, disp, paren, asList, commas)
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Types.Function as Func

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric
import Text.Printf
import qualified Blaze.Types.Pil.Checker as PI
import qualified Blaze.CallGraph as Cg

import qualified Data.HashMap.Strict as HashMap
import Blaze.Cfg (PilNode, CfNode (BasicBlock, Call, EnterFunc, LeaveFunc), BasicBlockNode, CallNode, EnterFuncNode, LeaveFuncNode, BranchType, PilEdge)
import qualified Blaze.Cfg as Cfg
import Blaze.Types.Graph.Unique (Unique)

-- TODO: make pretty return a monad instead of text,
-- which can do things like `indent`

-- class Pretty' t x | x -> t where
--   pretty' :: x -> Text

-- instance Pretty' Text Int where
--   pretty' n = show n

-- instance Pretty' () Int where
--   pretty' n = show (n + 1)

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
  pretty ys = "[" <> f ys <> "]"
    where
      f [] = ""
      f [x] = pretty x
      f (x : xs) = pretty x <> ", " <> f xs

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty m = "Map: " <> pretty (Map.toList m)

instance Pretty a => Pretty (Set a) where
  pretty ys = "#{" <> f (Set.toList ys) <> "}"
    where
      f [] = ""
      f [x] = pretty x
      f (x : xs) = pretty x <> ", " <> f xs

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
  pretty v = v ^. #symbol

instance Pretty Pil.OperationSize where
  pretty (Pil.OperationSize sz) = show sz

instance Pretty a => Pretty (Pil.CallDest a) where
  pretty dest = case dest of
    (Pil.CallAddr addr) -> show addr
    (Pil.CallExpr e) -> pretty e
    (Pil.CallExprs es) -> show $ fmap pretty es
    (Pil.CallFunc fn) -> pretty fn

instance Pretty Binja.Function.Function where
  pretty f = Text.pack $ printf "%s @0x%x" name start
    where
      name = f ^. Binja.Function.name
      start :: Word64
      (Address (Bytes start)) = f ^. Binja.Function.start

instance Pretty Pil.Ctx where
  pretty ctx = Text.pack $ printf "ctx (%s) %s" func idx
    where
      func :: Text
      func = pretty (ctx ^. #func)
      idx :: Text
      idx = show (ctx ^. #ctxIndex)

prettyBinop ::
  ( Pretty b,
    HasField' "left" a b,
    HasField' "right" a b
  ) =>
  Symbol ->
  a ->
  Text
prettyBinop sym op = Text.pack $ printf "%s (%s) (%s)" sym left right
  where
    left = pretty (op ^. #left)
    right = pretty (op ^. #right)

prettyUnop ::
  ( HasField' "src" a b,
    Pretty b
  ) =>
  Symbol ->
  a ->
  Text
prettyUnop sym op = Text.pack $ printf "%s(%s)" sym src
  where
    src = pretty (op ^. #src)

prettyConst ::
  ( HasField' "constant" a b,
    Show b
  ) =>
  a ->
  Text
prettyConst op = Text.pack $ printf "%s" const
  where
    const :: Text
    const = show (op ^. #constant)

prettyVar :: (HasField' "src" a Pil.PilVar) => a -> Text
prettyVar op = Text.pack $ printf "%s" src
  where
    src = pretty (op ^. #src)

prettyField ::
  ( HasField' "src" a b,
    Pretty b,
    HasField' "offset" a ByteOffset
  ) =>
  a ->
  Text
prettyField op = Text.pack $ printf "%s[%s]" src offset
  where
    src = pretty (op ^. #src)
    offset :: Text
    offset = show (op ^. #offset)

prettyExprOp :: Pretty a => Pil.ExprOp a -> Pil.OperationSize -> Text
prettyExprOp exprOp _size = case exprOp of
  (Pil.ADC op) -> prettyBinop "adc" op
  (Pil.ADD op) -> prettyBinop "add" op
  (Pil.ADD_OVERFLOW op) -> prettyBinop "addOf" op
  (Pil.AND op) -> prettyBinop "and" op
  (Pil.ASR op) -> prettyBinop "asr" op
  (Pil.BOOL_TO_INT op) -> prettyUnop "boolToInt" op
  (Pil.CEIL op) -> prettyUnop "ceil" op
  (Pil.CONST_BOOL op) -> "constBool" <-> show (op ^. #constant)
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
  (Pil.FIELD_ADDR op) ->
    "fieldAddr"
    <-> paren (pretty $ op ^. #baseAddr)
    <-> paren (pretty $ op ^. #offset)
  (Pil.CONST_FLOAT op) -> prettyConst op
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
  (Pil.LOAD op) -> Text.pack $ printf "[%s]" $ Text.unpack (pretty (op ^. #src))
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
  (Pil.STACK_LOCAL_ADDR op) -> "stackLocalAddr" <-> paren (pretty $ op ^. #stackOffset)
  (Pil.SUB op) -> prettyBinop "sub" op
  (Pil.SX op) -> prettyUnop "sx" op
  (Pil.TEST_BIT op) -> prettyBinop "testBit" op
  (Pil.UNIMPL t) -> "unimpl (" <> t <> ")"
  (Pil.UPDATE_VAR op) ->
    "updateVar"
    <-> paren (pretty $ op ^. #dest)
    <-> paren (pretty $ op ^. #offset)
    <-> paren (pretty $ op ^. #src)
  (Pil.VAR_PHI op) -> Text.pack $ printf "2%s <- %s" (pretty (op ^. #dest)) srcs
    where
      srcs :: Text
      srcs = show (fmap pretty (op ^. #src))
  (Pil.VAR_JOIN op) -> Text.pack $ printf "varJoin %s %s" (pretty (op ^. #high)) (pretty (op ^. #low))
  -- (Pil.VAR op) -> Text.pack $ printf "var \"%s\" %s" (pretty $ op ^. #src) (pretty)
  -- TODO: Need added
  (Pil.VAR op) -> prettyVar op
  -- TODO: Add field offset
  (Pil.VAR_FIELD op) -> prettyField op
  (Pil.XOR op) -> prettyBinop "xor" op
  (Pil.ZX op) -> prettyUnop "zx" op
  (Pil.CALL op) -> case op ^. #name of
    (Just name) -> Text.pack $ printf "call %s %s %s" name dest params
    Nothing -> Text.pack $ printf "call (Nothing) %s %s" dest params
    where
      dest = pretty (op ^. #dest)
      params :: Text
      params = asList (fmap pretty (op ^. #params))
  (Pil.StrCmp op) -> prettyBinop "strcmp" op
  (Pil.StrNCmp op) -> Text.pack $ printf "strncmp %d %s %s" (op ^. #len) (pretty (op ^. #left)) (pretty (op ^. #right))
  (Pil.MemCmp op) -> prettyBinop "memcmp" op
    -- TODO: Should ConstStr also use const rather than value as field name?
  (Pil.ConstStr op) -> Text.pack $ printf "constStr \"%s\"" $ op ^. #value
  (Pil.Extract op) -> Text.pack $ printf "extract %s %d" (pretty (op ^. #src)) (op ^. #offset)

instance Pretty PI.SymType where
  pretty (PI.SVar s) = pretty s
  pretty (PI.SType t) = pretty t

instance Pretty PI.DeepSymType where
  pretty (PI.DSVar s) = pretty s
  pretty (PI.DSType t) = pretty t
  pretty (PI.DSRecursive s dst) = paren ("Rec" <-> pretty s <> ":" <-> pretty dst)

instance Pretty PI.Sym where
  pretty (PI.Sym n) = "s" <> show n

instance Pretty (PI.InfoExpression (PI.SymInfo, Maybe PI.SymType)) where
  pretty (PI.InfoExpression (PI.SymInfo bitwidth s, mstype) op) =
--    "{" <> pretty s <> "}" <->
    prettyExprOp op (coerce $ bitwidth * 8) <->
    "::" <->
    paren (pretty s <-> "|" <-> maybe "Unknown" pretty mstype)

instance Pretty (PI.InfoExpression (PI.SymInfo, Maybe PI.DeepSymType)) where
  pretty (PI.InfoExpression (PI.SymInfo bitwidth s, mstype) op) =
--    "{" <> pretty s <> "}" <->
    prettyExprOp op (coerce $ bitwidth * 8) <->
    "::" <->
    paren (pretty s <-> "|" <-> maybe "Unknown" pretty mstype)


instance Pretty (PI.InfoExpression PI.SymInfo) where
  pretty (PI.InfoExpression (PI.SymInfo bitwidth (PI.Sym n)) op) =
    show n <> ":" <> paren (prettyExprOp op (coerce $ bitwidth * 8))

instance Pretty Pil.Expression where
  pretty (Pil.Expression size exprOp) = prettyExprOp exprOp size

instance (Pretty a, Pretty b) => Pretty (HashMap a b) where
  pretty m = "HashMap:\n"
    <> Text.intercalate "\n" (f <$> HashMap.toList m)
    where
      f (a, b) = "  " <> paren (pretty a <> "," <-> pretty b)

instance Pretty a => Pretty (Pil.Statement a) where
  pretty stmt = case stmt of
    Pil.Def x -> Text.pack $ printf "%s = %s" (pretty $ x ^. #var) (pretty $ x ^. #value)
    Pil.Constraint x -> Text.pack $ printf "?: %s" (pretty $ x ^. #condition)
    Pil.Store x -> Text.pack $ printf "[%s] = %s" (pretty $ x ^. #addr) (pretty $ x ^. #value)
    Pil.UnimplInstr t -> "Unimplemented Instruction (\"" <> t <> "\")"
    Pil.UnimplMem x -> Text.pack $ printf "Unimplemented Memory: [%s]" (pretty $ x ^. #src)
    Pil.Undef -> "Undefined"
    Pil.Nop -> "Nop"
    Pil.Annotation t -> "Annotation: " <> t
    Pil.EnterContext x -> "----> Entering " <> pretty (x ^. #ctx)
    Pil.ExitContext x -> "<---- Leaving " <> pretty (x ^. #leavingCtx)
    Pil.Call x -> Text.pack $ printf "%s (\n%s\n)" (pretty $ x ^. #dest) (pretty $ x ^. #params)
    Pil.DefPhi x -> Text.pack $ printf "%s = %s"
                      (pretty $ x ^. #dest)
                      (asList . fmap pretty $ x ^. #src)
    Pil.BranchCond x -> "Branch cond | " <> pretty (x ^. #cond)
    Pil.Ret x -> "Ret " <> pretty (x ^. #value)
    Pil.Exit -> "Exit"
    Pil.TailCall x -> "Tail call | " <> pretty (x ^. #dest) <> "(" <> pretty (x ^. #args) <> ")"

instance Pretty Pil.CallSite where
  pretty x = pretty (x ^. #caller) <> " -> "
             <> pretty (x ^. #callDest)

newtype PStmts a = PStmts [Pil.Statement a]

newtype PIndexedStmts a = PIndexedStmts [(Int, Pil.Statement a)]

instance Pretty a => Pretty (PStmts a) where
  pretty (PStmts stmts) = Text.intercalate "\n" . fmap pretty $ stmts

instance Pretty a => Pretty (PIndexedStmts a) where
  pretty (PIndexedStmts stmts) = Text.intercalate "\n" . fmap f $ stmts
    where
      f (i, stmt) = show i <> ":" <-> pretty stmt

instance Pretty ByteOffset where
  pretty = disp

instance Pretty Pil.StackOffset where
  pretty x =
    "stackOffset"
      <-> show (x ^. #offset)
      <-> paren (pretty (x ^. #ctx))

instance Pretty Pil.StmtIndex where
  pretty x = show x

instance Pretty t => Pretty (PI.PilType t) where
  pretty = \case
    PI.TArray len elemType -> "Array" <-> pretty len <-> pretty elemType
--    PI.TZeroField pt -> "ZeroField" <-> paren (pretty pt)
    PI.TBool -> "Bool"
    PI.TChar -> "Char"
    -- PI.TQueryChar -> "QueryChar"
    PI.TInt bitWidth signed -> "Int" <-> pretty bitWidth <-> pretty signed
    PI.TFloat bitWidth -> "Float" <-> pretty bitWidth
    PI.TBitVector bitWidth -> "BitVector" <-> pretty bitWidth
    PI.TPointer bitWidth pointeeType -> "Pointer" <-> pretty bitWidth
                                        <-> paren (pretty pointeeType)
    PI.TRecord m -> "Record" <-> asList (rfield <$> HashMap.toList m)
      where
        rfield (BitOffset n, t) = paren $ commas [show n, pretty t]

    PI.TBottom s -> paren $ "Bottom" <-> pretty s
    PI.TFunction _ret _params -> "Func"

    PI.TVBitWidth (Bits bitWidth) -> show bitWidth <> "w"
    PI.TVLength n -> show n
    PI.TVSign b -> if b then "Signed" else "Unsigned"

--- CallGraph
instance Pretty Cg.CallDest where
  pretty (Cg.DestFunc x) = pretty x

instance Pretty Cg.CallSite where
  pretty x =
    pretty (x ^. #caller) 
      <> "@" <> pretty (x ^. #address)
      <> " -> " <> pretty (x ^. #dest)

--- Function
instance Pretty Func.Function where
  pretty (Func.Function _ name _addr _) = name -- <> "@" <> showHex addr


-- not really sure how to display this.
-- I don't really want to show the whole NodeId
instance Pretty n => Pretty (Unique n) where
  pretty x = "U" <> (paren . pretty $ x ^. #node)

--- CFG
instance Pretty PilNode where
  pretty = \case
    BasicBlock n -> pretty n
    Call n -> pretty n
    EnterFunc n -> pretty n
    LeaveFunc n -> pretty n

instance Pretty PilEdge where
  pretty e =
    pretty (e ^. #edge . #src)
      <> " ---> " <> pretty (e ^. #edge . #dst) 
      <> "  |" <> pretty (e ^. #label) <> "|"

instance Pretty (BasicBlockNode a) where
  pretty (Cfg.BasicBlockNode f start end _) =
    pretty f
      <> "@[" <> pretty start <> ", " <> pretty end <> "]"

instance Pretty (CallNode a) where
  pretty (Cfg.CallNode f start _) =
    pretty f
      <> "@" <> pretty start

instance Pretty (EnterFuncNode a) where
  pretty (Cfg.EnterFuncNode prevCtx nextCtx _) =
    "EnterFunc Ctx: " <> pretty prevCtx <> " -> " <> pretty nextCtx 

instance Pretty (LeaveFuncNode a) where
  pretty (Cfg.LeaveFuncNode prevCtx nextCtx _) =
    "LeaveFunc Ctx: " <> pretty prevCtx <> " -> " <> pretty nextCtx 

instance Pretty BranchType where
  pretty = show

--- Path
-- instance Pretty Path.Node where
--   pretty (Path.SubBlock x) =
--     bracket (pretty (x ^. #start) <> "-" <> pretty (x ^. #end - 1)) <> " : SubBlock"
--   pretty (Path.Call x) =
--     "-------Expanding call: " <> pretty (x ^. #callSite)
--   pretty (Path.Ret x) =
--     "-------Returning to " <> pretty (x ^. #func) <> " from " <> pretty (x ^. #callSite . #callDest)
--   pretty (Path.AbstractCall x) = pretty x
--   pretty (Path.AbstractPath _) = "AbstractPath"
--   pretty (Path.Condition x) =
--     "Condition: " <> bool "NOT " "" (x ^. #trueOrFalseBranch)
--     <> pretty (x ^. #condition)

-- instance Pretty Path.AbstractCallNode where
--   pretty x = bracket (pretty $ x ^. #callSite . #callInstr . #index)
--     <> " : "
--     <> pretty (x ^. #callSite)


--- AlgaPath
-- instance Pretty AlgaPath.AlgaPath where
--   pretty p = case uncons (Path.toList p) of
--     Nothing -> ""
--     Just (x, xs) ->
--       "========= Starting in: " <> pretty (Path.getNodeFunc x) <> " =========\n"
--         <> f (x : xs)
--     where
--       f [] = ""
--       f (x : xs) = pretty x <> "\n" <> f xs

-- instance Pretty (AlgaGraph () Func.Function) where
--   pretty = asMultilineList . fmap (ptup . snd) . G.edges
--     where
--       ptup (a, b) = paren $ pretty a <-> "->" <-> pretty b

prettyStmts :: (MonadIO m, Pretty a) => [Pil.Statement a] -> m ()
prettyStmts = prettyPrint . PStmts

prettyIndexedStmts :: (MonadIO m, Pretty a) => [(Int, Pil.Statement a)] -> m ()
prettyIndexedStmts = prettyPrint . PIndexedStmts

-- | Pretty print to IO.
prettyPrint :: (MonadIO m, Pretty a) => a -> m ()
prettyPrint = putText . pretty

pp :: (MonadIO m, Pretty a) => a -> m ()
pp = prettyPrint
