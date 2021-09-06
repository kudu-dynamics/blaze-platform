{-# LANGUAGE UndecidableInstances #-}
{- HLINT ignore "Use :" -}

module Blaze.Pretty
  ( TokenType(..),
    TokenContext(..),
    Token(..),
    Tokenizable(tokenize),
    plainToken,
    varToken,
    addressToken,
    stringToken,
    textToken,
    keywordToken,
    paren,
    delimitedList,
    tokenizeAsList,
    tokenizeAsTuple,
    tokenizeAsCurlyList,
    pretty,
    prettyPrint,
    pp,
    PStmts(PStmts),
    prettyStmts,
    prettyIndexedStmts,
    showHex,
    PrettyShow(PrettyShow),
  )
where

import Blaze.Prelude hiding (Symbol, const, sym, bracket)
import qualified Prelude (show)
import qualified Binja.Function
import qualified Binja.MLIL as MLIL
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Types.Function as Func

import qualified Data.Map as Map
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Numeric
import qualified Blaze.Types.Pil.Checker as PI
import qualified Blaze.CallGraph as Cg
import qualified Blaze.Graph as G

import qualified Data.HashMap.Strict as HashMap
import Blaze.Cfg (CfNode (BasicBlock, Call, EnterFunc, LeaveFunc), BasicBlockNode, CallNode, EnterFuncNode, LeaveFuncNode, BranchType, CfEdge, Cfg)
import Blaze.Types.Cfg.Interprocedural (InterCfg(InterCfg))
import qualified Blaze.Cfg as Cfg
import Blaze.Pil.Display (needsParens)
-- TODO: make tokenize return a monad instead of text,
-- which can do things like `indent`

-- class Tokenizable' t x | x -> t where
--   tokenize' :: x -> Text

-- instance Tokenizable' Text Int where
--   tokenize' n = show n

-- instance Tokenizable' () Int where
--   tokenize' n = show (n + 1)

data TokenType =
    TextToken
  | InstructionToken
  | OperandSeparatorToken
  | RegisterToken
  | IntegerToken
  | PossibleAddressToken
  | BeginMemoryOperandToken
  | EndMemoryOperandToken
  | FloatingPointToken
  | AnnotationToken
  | CodeRelativeAddressToken
  | ArgumentNameToken
  | HexDumpByteValueToken
  | HexDumpSkippedByteToken
  | HexDumpInvalidByteToken
  | HexDumpTextToken
  | OpcodeToken
  | StringToken
  | CharacterConstantToken
  | KeywordToken
  | TypeNameToken
  | FieldNameToken
  | NameSpaceToken
  | NameSpaceSeparatorToken
  | TagToken
  | StructOffsetToken
  | StructOffsetByteValueToken
  | StructureHexDumpTextToken
  | GotoLabelToken
  | CommentToken
  | PossibleValueToken
  | PossibleValueTypeToken
  | ArrayIndexToken
  | IndentationToken
  | CodeSymbolToken
  | DataSymbolToken
  | LocalVariableToken
  | ImportToken
  | AddressDisplayToken
  | IndirectImportToken
  | ExternalSymbolToken
  deriving (Eq, Ord, Show, Enum, Generic, FromJSON, ToJSON)

data TokenContext =
    NoTokenContext
 |  LocalVariableTokenContext
 |  DataVariableTokenContext
 |  FunctionReturnTokenContext
 |  InstructionAddressTokenContext
 |  ILInstructionIndexTokenContext
 deriving (Eq, Ord, Show, Enum, Generic, FromJSON, ToJSON)

data Token = Token
  { tokenType :: TokenType
  , text :: Text
  , value :: Integer
  , size :: Int
  , operand :: Integer
  , context :: TokenContext
  -- , confidence :: Int
  , address :: Address
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

class Tokenizable a where
  tokenize :: a -> [Token]

plainToken :: TokenType -> Text -> Token
plainToken typ t = Token
  { tokenType = typ
  , text = t
  , value = 0
  , size = 0
  , operand = 0xffffffff
  , context = NoTokenContext
  , address = 0
  }

integerToken :: Integral n => n -> Token
integerToken n = Token
  { tokenType = IntegerToken
  , text = showHex n'
  , value = n'
  , size = 0
  , operand = 0xffffffff
  , context = NoTokenContext
  , address = 0
  }
  where
    n' = toInteger n

-- XXX Figure out value and operand here
varToken :: Text -> Token
varToken t = Token
  { tokenType = LocalVariableToken
  , text = t
  , value = 0
  , size = 0
  , operand = 0
  , context = LocalVariableTokenContext
  , address = 0
  }

addressToken :: Maybe Text -> Address -> Token
addressToken t addr = Token
  { tokenType = PossibleAddressToken
  , text = fromMaybe (showHex addr) t
  , value = toInteger addr
  , size = 0
  , operand = 0xffffffff
  , context = InstructionAddressTokenContext
  , address = 0
  }

stringToken :: Text -> Token
stringToken = plainToken StringToken . show

textToken :: Text -> Token
textToken = plainToken TextToken

tt :: Text -> Token
tt = textToken

keywordToken :: Text -> Token
keywordToken = plainToken KeywordToken

paren :: [Token] -> [Token]
paren ts = [tt "("] ++ ts ++ [tt ")"]

delimitedList :: [Token] -> [Token] -> [Token] -> [[Token]] -> [Token]
delimitedList start sep end xs = start ++ concat (intersperse sep xs) ++ end

tokenizeAsList :: Tokenizable a => [a] -> [Token]
tokenizeAsList = delimitedList [tt "["] [tt ", "] [tt "]"] . fmap tokenize

tokenizeAsTuple :: Tokenizable a => [a] -> [Token]
tokenizeAsTuple = delimitedList [tt "("] [tt ", "] [tt ")"] . fmap tokenize

tokenizeAsCurlyList :: Tokenizable a => [a] -> [Token]
tokenizeAsCurlyList = delimitedList [tt "{"] [tt ", "] [tt "}"] . fmap tokenize

pretty :: Tokenizable a => a -> Text
pretty a = foldMap (view #text) $ tokenize a

showHex :: (Integral a, Show a) => a -> Text
showHex x
  | x < 0 = Text.pack $ "-0x" <> Numeric.showHex (abs x) ""
  | otherwise = Text.pack $ "0x" <> Numeric.showHex x ""

showStackLocalByteOffset :: ByteOffset -> Text
showStackLocalByteOffset x = bool "arg_" "var_" (x < 0)
  <> (Text.pack . flip Numeric.showHex "" . abs $ x)

parenExpr :: (Tokenizable a, HasField' "op" a (Pil.ExprOp a)) => a -> [Token]
parenExpr x = if needsParens $ x ^. #op
  then paren (tokenize x)
  else tokenize x

instance Tokenizable a => Tokenizable [a] where
  tokenize = tokenizeAsList

instance Tokenizable () where
  tokenize () = [tt "()"]

instance Tokenizable Token where
  tokenize t = [t]

instance (Tokenizable k, Tokenizable v) => Tokenizable (Map k v) where
  tokenize m = [keywordToken "Map: "] ++ tokenizeAsList (Map.toList m)

instance Tokenizable a => Tokenizable (HashSet a) where
  tokenize = delimitedList [tt "#{"] [tt ", "] [tt "}"] . fmap tokenize . HashSet.toList

instance Tokenizable (MLIL.Expression a) where
  tokenize _ = [tt "(TODO: MLIL Expression)"]

instance (Tokenizable a, Tokenizable b) => Tokenizable (a, b) where
  tokenize (a, b) = paren $ tokenize a ++ [tt ", "] ++ tokenize b

---- PIL

instance Tokenizable a => Tokenizable (Pil.CallDest a) where
  tokenize dest = case dest of
    (Pil.CallAddr fptr) -> tokenize fptr
    (Pil.CallExpr e) -> tokenize e
    (Pil.CallExprs es) -> tokenizeAsList es
    (Pil.CallFunc fn) -> tokenize fn
    (Pil.CallExtern x) -> tokenize x

instance Tokenizable Binja.Function.Function where
  tokenize f =
    [ Token
      { tokenType = CodeSymbolToken
      , text = f ^. Binja.Function.name
      , value = toInteger start
      , size = 0
      , operand = 0xffffffff
      , context = NoTokenContext
      , address = 0
      }
    ]
    where
      start :: Word64
      (Address (Bytes start)) = f ^. Binja.Function.start

instance Tokenizable Pil.Ctx where
  tokenize ctx =
    [keywordToken "ctx", tt " "] ++
    paren func ++
    [textToken $ show (ctx ^. #ctxId)]
    where
      func :: [Token]
      func = tokenize (ctx ^. #func)

instance Tokenizable Pil.PilVar where
  tokenize var = [varToken (var ^. #symbol)]

tokenizeBinop
  :: ( Tokenizable b
     , HasField' "left" a b
     , HasField' "right" a b
     , HasField' "op" b (Pil.ExprOp b)
     )
  => Text
  -> a
  -> [Token]
tokenizeBinop sym op = [tt $ sym <> " "] ++ left ++ [tt " "] ++ right
  where
    left = parenExpr (op ^. #left)
    right = parenExpr (op ^. #right)

tokenizeBinopInfix
  :: ( Tokenizable b
     , HasField' "left" a b
     , HasField' "right" a b
     , HasField' "op" b (Pil.ExprOp b)
     )
  => Text
  -> a
  -> [Token]
tokenizeBinopInfix sym op = left ++ [tt $ " " <> sym <> " "] ++ right
  where
    left = parenExpr (op ^. #left)
    right = parenExpr (op ^. #right)

tokenizeUnop
  :: ( HasField' "src" a b
     , Tokenizable b
     , HasField' "op" b (Pil.ExprOp b)
     )
  => Text
  -> a
  -> [Token]
tokenizeUnop sym op = [tt $ sym <> " "] ++ src
  where
    src = parenExpr (op ^. #src)

tokenizeField ::
  ( HasField' "src" a b
  , Tokenizable b
  , HasField' "offset" a ByteOffset
  )
  => a
  -> [Token]
tokenizeField op = src ++ [tt "["] ++ offset ++ [tt "]"]
  where
    src = tokenize (op ^. #src)
    offset = tokenize (op ^. #offset)

tokenizeExprOp ::
  (Tokenizable a, HasField' "op" a (Pil.ExprOp a))
  => Pil.ExprOp a
  -> Pil.OperationSize
  -> [Token]
tokenizeExprOp exprOp _size = case exprOp of
  (Pil.ADC op) -> tokenizeBinop "adc" op
  (Pil.ADD op) -> tokenizeBinopInfix "+" op
  (Pil.ADD_OVERFLOW op) -> tokenizeBinopInfix "&+" op
  (Pil.AND op) -> tokenizeBinopInfix "&&" op
  (Pil.ASR op) -> tokenizeBinop "asr" op
  (Pil.BOOL_TO_INT op) -> tokenizeUnop "boolToInt" op
  (Pil.CEIL op) -> tokenizeUnop "ceil" op
  (Pil.CONST_BOOL op) -> [tt . show $ op ^. #constant]
  (Pil.CMP_E op) -> tokenizeBinopInfix "==" op
  (Pil.CMP_NE op) -> tokenizeBinopInfix "!=" op
  (Pil.CMP_SGE op) -> tokenizeBinopInfix ">=" op
  (Pil.CMP_SGT op) -> tokenizeBinopInfix ">" op
  (Pil.CMP_SLE op) -> tokenizeBinopInfix "<=" op
  (Pil.CMP_SLT op) -> tokenizeBinopInfix "<" op
  (Pil.CMP_UGE op) -> tokenizeBinopInfix "u>=" op
  (Pil.CMP_UGT op) -> tokenizeBinopInfix "u>" op
  (Pil.CMP_ULE op) -> tokenizeBinopInfix "u<=" op
  (Pil.CMP_ULT op) -> tokenizeBinopInfix "u<" op
  (Pil.CONST op) -> [integerToken (op ^. #constant)]
  (Pil.CONST_PTR op) -> [addressToken Nothing $ fromIntegral (op ^. #constant)]
  (Pil.DIVS op) -> tokenizeBinopInfix "/" op
  (Pil.DIVS_DP op) -> tokenizeBinop "divsDP" op
  (Pil.DIVU op) -> tokenizeBinopInfix "u/" op
  (Pil.DIVU_DP op) -> tokenizeBinop "divuDP" op
  (Pil.FABS op) -> tokenizeUnop "fabs" op
  (Pil.FADD op) -> tokenizeBinopInfix "f+" op
  (Pil.FCMP_E op) -> tokenizeBinopInfix "f==" op
  (Pil.FCMP_GE op) -> tokenizeBinopInfix "f>=" op
  (Pil.FCMP_GT op) -> tokenizeBinopInfix "f>" op
  (Pil.FCMP_LE op) -> tokenizeBinopInfix "f<=" op
  (Pil.FCMP_LT op) -> tokenizeBinopInfix "f<" op
  (Pil.FCMP_NE op) -> tokenizeBinopInfix "f!=" op
  (Pil.FCMP_O op) -> tokenizeBinop "fcmpO" op
  (Pil.FCMP_UO op) -> tokenizeBinop "fcmpUO" op
  (Pil.FDIV op) -> tokenizeBinopInfix "f/" op
  (Pil.FIELD_ADDR op) ->
    parenExpr (op ^. #baseAddr) ++
    [tt " + ", keywordToken "offset", tt " "] ++
    tokenize (op ^. #offset)
  (Pil.CONST_FLOAT op) -> [plainToken FloatingPointToken $ show (op ^. #constant)]
  (Pil.FLOAT_CONV op) -> tokenizeUnop "floatConv" op
  (Pil.FLOAT_TO_INT op) -> tokenizeUnop "floatToInt" op
  (Pil.FLOOR op) -> tokenizeUnop "floor" op
  (Pil.FMUL op) -> tokenizeBinopInfix "f*" op
  (Pil.FNEG op) -> tokenizeUnop "fneg" op
  (Pil.FSQRT op) -> tokenizeUnop "fsqrt" op
  (Pil.FSUB op) -> tokenizeBinopInfix "f-" op
  (Pil.FTRUNC op) -> tokenizeUnop "ftrunc" op
  (Pil.IMPORT op) -> [addressToken Nothing $ fromIntegral (op ^. #constant)]
  (Pil.INT_TO_FLOAT op) -> tokenizeUnop "intToFloat" op
  (Pil.LOAD op) -> [tt "["]  ++ tokenize (op ^. #src) ++ [tt "]"]
  -- TODO: add memory versions for all SSA ops
  (Pil.LOW_PART op) -> tokenizeUnop "lowPart" op
  (Pil.LSL op) -> tokenizeBinop "lsl" op
  (Pil.LSR op) -> tokenizeBinop "lsr" op
  (Pil.MODS op) -> tokenizeBinopInfix "%" op
  (Pil.MODS_DP op) -> tokenizeBinop "modsDP" op
  (Pil.MODU op) -> tokenizeBinopInfix "u%" op
  (Pil.MODU_DP op) -> tokenizeBinop "moduDP" op
  (Pil.MUL op) -> tokenizeBinopInfix "*" op
  (Pil.MULS_DP op) -> tokenizeBinop "mulsDP" op
  (Pil.MULU_DP op) -> tokenizeBinop "muluDP" op
  (Pil.NEG op) -> tokenizeUnop "neg" op
  (Pil.NOT op) -> tokenizeUnop "not" op
  (Pil.OR op) -> tokenizeBinopInfix "|" op
  -- TODO: Need to add carry
  (Pil.RLC op) -> tokenizeBinop "rlc" op
  (Pil.ROL op) -> tokenizeBinop "rol" op
  (Pil.ROR op) -> tokenizeBinop "ror" op
  (Pil.ROUND_TO_INT op) -> tokenizeUnop "roundToInt" op
  -- TODO: Need to add carry
  (Pil.RRC op) -> tokenizeBinop "rrc" op
  (Pil.SBB op) -> tokenizeBinop "sbb" op
  (Pil.STACK_LOCAL_ADDR op) ->
    [ tt "&"
    , varToken $ showStackLocalByteOffset (op ^. #stackOffset . #offset)
    ]
  (Pil.SUB op) -> tokenizeBinopInfix "-" op
  (Pil.SX op) -> tokenizeUnop "sx" op
  (Pil.TEST_BIT op) -> tokenizeBinop "testBit" op
  (Pil.UNIMPL t) -> [keywordToken "unimpl"] ++ paren [tt t]
  (Pil.UPDATE_VAR op) ->
    [keywordToken "updateVar"] ++
    paren
      ( arg "var" ++
        [varToken $ op ^. #dest . #symbol] ++
        [tt ", "] ++
        arg "offset" ++
        tokenize (op ^. #offset) ++
        [tt ", "] ++
        arg "val" ++
        tokenize (op ^. #src)
      )
    where
      arg a = [plainToken ArgumentNameToken a, tt ": "]
  (Pil.VAR_PHI op) -> [tt (op ^. #dest ^. #symbol), tt " <- "] ++ srcs
    where
      srcs :: [Token]
      srcs = tt . view #symbol <$> (op ^. #src)
  (Pil.VAR_JOIN op) ->
    [tt "varJoin "] ++
    tokenize (op ^. #high) ++
    [tt " "] ++
    tokenize (op ^. #low)
  -- TODO: Need added
  (Pil.VAR op) -> tokenize (op ^. #src)
  -- TODO: Add field offset
  (Pil.VAR_FIELD op) -> tokenizeField op
  (Pil.XOR op) -> tokenizeBinop "xor" op
  (Pil.ZX op) -> tokenizeUnop "zx" op
  (Pil.CALL op) -> tokenize op
  (Pil.StrCmp op) -> tokenizeBinop "strcmp" op
  (Pil.StrNCmp op) ->
    [tt "strncmp ", integerToken (op ^. #len), tt " "] ++
    tokenize (op ^. #left) ++
    [tt " "] ++
    tokenize (op ^. #right)
  (Pil.MemCmp op) -> tokenizeBinop "memcmp" op
  (Pil.ExternPtr op) -> tokenize op
    -- TODO: Should ConstStr also use const rather than value as field name?
  (Pil.ConstStr op) -> [stringToken $ op ^. #value]
  (Pil.ConstFuncPtr op) -> tokenize op
  (Pil.Extract op) ->
    [keywordToken "extract", tt " "] ++
    tokenize (op ^. #src) ++
    [tt " "] ++
    tokenize (op ^. #offset)
  Pil.UNIT -> [tt "()"]

instance Tokenizable PI.SymType where
  tokenize (PI.SVar s) = tokenize s
  tokenize (PI.SType t) = tokenize t

instance Tokenizable PI.DeepSymType where
  tokenize (PI.DSVar s) = tokenize s
  tokenize (PI.DSType t) = tokenize t
  tokenize (PI.DSRecursive s dst) =
    paren $
    [keywordToken "Rec", tt " "] ++
    tokenize s ++
    [tt ": "] ++
    tokenize dst

instance Tokenizable PI.Sym where
  tokenize (PI.Sym n) = [tt $ "s" <> show n]

instance Tokenizable (PI.InfoExpression (PI.SymInfo, Maybe PI.SymType)) where
  tokenize (PI.InfoExpression (PI.SymInfo bitwidth s, mstype) op) =
    tokenizeExprOp op (coerce $ bitwidth * 8) ++
    [tt " :: "] ++
    paren
      ( tokenize s ++
        [tt " | "] ++
        maybe [keywordToken "Unknown"] tokenize mstype
      )

instance Tokenizable (PI.InfoExpression (PI.SymInfo, Maybe PI.DeepSymType)) where
  tokenize (PI.InfoExpression (PI.SymInfo bitwidth s, mstype) op) =
    tokenizeExprOp op (coerce $ bitwidth * 8) ++
    [tt " :: "] ++
    paren
     (  tokenize s ++
        [tt " | "] ++
        maybe [keywordToken "Unknown"] tokenize mstype
     )

instance Tokenizable (PI.InfoExpression PI.SymInfo) where
  tokenize (PI.InfoExpression (PI.SymInfo bitwidth (PI.Sym n)) op) =
    [tt (show n), tt ":"] ++ paren (tokenizeExprOp op (coerce $ bitwidth * 8))

instance Tokenizable Pil.Expression where
  tokenize (Pil.Expression size' exprOp) = tokenizeExprOp exprOp size'

instance (Tokenizable a, Tokenizable b) => Tokenizable (HashMap a b) where
  tokenize m =
    [keywordToken "HashMap:", tt "\n"] ++ intercalate [tt "\n"] (f <$> HashMap.toList m)
    where
      f (a, b) = [tt "  "] ++ paren (tokenize a ++ [tt ", "] ++ tokenize b)

instance (Tokenizable a, HasField' "op" a (Pil.ExprOp a)) => Tokenizable (Pil.Statement a) where
  tokenize stmt = case stmt of
    Pil.Def x -> tokenize (x ^. #var) ++ [tt " = "] ++ tokenize (x ^. #value)
    Pil.Constraint x -> [tt "?: "] ++ tokenize (x ^. #condition)
    Pil.Store x ->
      [tt "["] ++
      tokenize (x ^. #addr) ++
      [tt "]", tt " = "] ++
      tokenize (x ^. #value)
    Pil.UnimplInstr t ->
      let shortRepr = show $ Text.take 10 t in
        [ tt "Unimplemented Instruction (\""
        , tt (Text.dropEnd 1 . Text.drop 1 $ shortRepr)
        , tt "..."
        , tt "\")"
        ]
    Pil.UnimplMem x -> [tt "Unimplemented Memory: ", tt "["] ++ tokenize (x ^. #src)
    Pil.Undef -> [keywordToken "Undefined"]
    Pil.Nop -> [keywordToken "Nop"]
    Pil.Annotation t -> [plainToken CommentToken "// ", plainToken CommentToken t]
    Pil.EnterContext x -> [tt "----> Entering "] ++ tokenize (x ^. #ctx)
    Pil.ExitContext x -> [tt "<---- Leaving "] ++ tokenize (x ^. #leavingCtx)
    Pil.Call callOp -> tokenize callOp
    Pil.DefPhi x ->
      tokenize (x ^. #dest) ++
      [tt " = ", keywordToken "φ"] ++
      tokenizeAsCurlyList (x ^. #src)
    Pil.DefMemPhi x ->
      [varToken ("mem#" <> show (x ^. #destMemory)), tt " = ", tt "φ"] ++
      tokenizeAsCurlyList ((\m -> varToken $ "mem#" <> show m) <$> (x ^. #srcMemory))
    Pil.BranchCond x -> [keywordToken "if", tt " "] ++ paren (tokenize $ x ^. #cond)
    Pil.Jump x -> [keywordToken "jump", tt " "] ++ parenExpr (x ^. #dest)
    Pil.JumpTo x ->
      [keywordToken "jumpTo", tt " "] ++
      parenExpr (x ^. #dest) ++
      tokenizeAsList (x ^. #targets)
    Pil.Ret x -> [keywordToken "return", tt " "] ++ tokenize (x ^. #value)
    Pil.NoRet -> [keywordToken "NoRet"]
    Pil.Exit -> [keywordToken "Exit"]
    Pil.TailCall x ->
      [keywordToken "Tail call", tt " | "] ++ tokenize (x ^. #dest) ++ tokenizeAsList (x ^. #args)

instance Tokenizable a => Tokenizable (Pil.CallOp a) where
  tokenize op = case (op ^. #name, op ^. #dest) of
    (Just name, Pil.CallAddr fptr) ->
      [addressToken (Just name) $ fptr ^. #address] ++ params
    _ -> [keywordToken "call", tt " "] ++ dest ++ params
    where
      dest = tokenize (op ^. #dest)
      params :: [Token]
      params = tokenizeAsTuple (op ^. #params)

instance Tokenizable Pil.CallSite where
  tokenize x = tokenize (x ^. #caller) ++ [tt " -> "] ++ tokenize (x ^. #callDest)

newtype PStmts a = PStmts [Pil.Statement a]

newtype PIndexedStmts a = PIndexedStmts [(Int, Pil.Statement a)]

instance (Tokenizable a, HasField' "op" a (Pil.ExprOp a)) => Tokenizable (PStmts a) where
  tokenize (PStmts stmts) = intercalate [tt "\n"] (tokenize <$> stmts)

instance (Tokenizable a, HasField' "op" a (Pil.ExprOp a)) => Tokenizable (PIndexedStmts a) where
  tokenize (PIndexedStmts stmts) = intercalate [tt "\n"] (f <$> stmts)
    where
      f :: (Int, Pil.Statement a) -> [Token]
      f (i, stmt) = [integerToken i, tt ": "] ++ tokenize stmt

instance Tokenizable ByteOffset where
  tokenize (ByteOffset n) = [integerToken n]

instance Tokenizable Address where
  tokenize (Address n) = [integerToken n]

instance Tokenizable Int64 where
  tokenize n = [integerToken n]

instance Tokenizable Pil.StackOffset where
  tokenize x =
    [tt "stackOffset "] ++ tokenize (x ^. #offset) ++ paren (tokenize (x ^. #ctx))

instance Tokenizable Pil.StmtIndex where
  tokenize x = [integerToken (x ^. #val)]

instance Tokenizable t => Tokenizable (PI.PilType t) where
  tokenize = \case
    PI.TArray len elemType -> [keywordToken "Array", tt " "] ++ tokenize len ++ [tt " "] ++ tokenize elemType
--    PI.TZeroField pt -> "ZeroField" <-> paren (tokenize pt)
    PI.TBool -> [keywordToken "Bool"]
    PI.TChar -> [keywordToken "Char"]
    -- PI.TQueryChar -> "QueryChar"
    PI.TInt bitWidth signed -> [keywordToken "Int", tt " "] ++ tokenize bitWidth ++ [tt " "] ++ tokenize signed
    PI.TFloat bitWidth -> [keywordToken "Float", tt " "] ++ tokenize bitWidth
    PI.TBitVector bitWidth -> [keywordToken "BitVector", tt " "] ++ tokenize bitWidth
    PI.TPointer bitWidth pointeeType ->
      [keywordToken "Pointer", tt " "] ++ tokenize bitWidth ++ [tt " "] ++ paren (tokenize pointeeType)
    PI.TRecord m ->
      [keywordToken "Record", tt " "] ++
      delimitedList [tt "["] [tt ", "] [tt "]"] (rfield <$> HashMap.toList m)
      where
        rfield (BitOffset n, t) = paren ([tt (show n), tt ", "] ++ tokenize t)

    PI.TBottom s -> paren ([keywordToken "Bottom", tt " "] ++ tokenize s)
    PI.TUnit -> [keywordToken "Unit"]
    PI.TFunction _ret _params -> [keywordToken "Func"]

    PI.TVBitWidth (Bits bitWidth) -> [tt $ show bitWidth <> "w"]
    PI.TVLength n -> [integerToken n]
    PI.TVSign b -> [keywordToken (if b then "Signed" else "Unsigned")]

--- CallGraph
instance Tokenizable Cg.CallDest where
  tokenize (Cg.DestFunc x) = tokenize x

instance Tokenizable Cg.CallSite where
  tokenize x = tokenize (x ^. #caller) ++ [tt " -> "] ++ tokenize (x ^. #dest)

--- Function
instance Tokenizable Func.Function where
  tokenize (Func.Function _ name addr _) =
    [ Token
      { tokenType = CodeSymbolToken
      , text = name
      , value = toInteger addr
      , size = 0
      , operand = 0xffffffff
      , context = NoTokenContext
      , address = 0
      }
    ]

instance Tokenizable Pil.ConstFuncPtrOp where
  tokenize x = [addressToken (x ^. #symbol) (x ^. #address)]

instance Tokenizable Pil.ExternPtrOp where
  tokenize x = [ tt "extern ", addressToken (x ^. #symbol) addr ]
               <> moff
    where
      moff = case x ^. #offset of
        0 -> []
        n -> [tt "[", integerToken n, tt "]"]
      addr = x ^. #address

--- CFG
instance Tokenizable (CfNode a) where
  tokenize = \case
    BasicBlock n -> tokenize n
    Call n -> tokenize n
    EnterFunc n -> tokenize n
    LeaveFunc n -> tokenize n

instance Tokenizable (CfEdge a) where
  tokenize e =
    tokenize (e ^. #src) ++
    [tt " ---> "] ++
    tokenize (e ^. #dst) ++
    [tt "  |"] ++
    tokenize (e ^. #branchType) ++
    [tt "|"]

instance Tokenizable (BasicBlockNode a) where
  tokenize (Cfg.BasicBlockNode ctx start end _ _) =
    tokenize ctx ++
    [ tt "@["
    , addressToken Nothing start
    , tt ", "
    , addressToken Nothing end
    , tt "]"
    ]

instance Tokenizable (CallNode a) where
  tokenize (Cfg.CallNode ctx start dest _ _) =
    tokenize ctx ++
    [ tt "@"
    , addressToken Nothing start
    , tt " -> "
    ] ++
    tokenize dest

instance Tokenizable (EnterFuncNode a) where
  tokenize (Cfg.EnterFuncNode prevCtx nextCtx _ _) =
    [tt "EnterFunc Ctx: "] ++
    tokenize prevCtx ++
    [tt " -> "] ++
    tokenize nextCtx

instance Tokenizable (LeaveFuncNode a) where
  tokenize (Cfg.LeaveFuncNode prevCtx nextCtx _ _) =
    [tt "LeaveFunc Ctx: "] ++
    tokenize prevCtx ++
    [tt " -> "] ++
    tokenize nextCtx

instance Tokenizable BranchType where
  tokenize bt = [tt (show bt)]

-- instance Tokenizable [Pil.Stmt] where
--   tokenize = intercalate [tt "\n"] . fmap tokenize

instance Tokenizable InterCfg where
  tokenize (InterCfg cfg) = tokenize cfg
  
-- | This matches each node to an Int and uses the Int to show the edges
instance Tokenizable a => Tokenizable (Cfg a) where
  tokenize cfg =
    [tt "---CFG---\n", tt "--- Node Mapping:\n"] ++
    showNodeMapping ++
    [tt "--- Edges:\n"] ++
    showEdges ++
    [tt "--- Attrs:\n"] ++
    showAttrs
    where
      cflow = cfg ^. #graph

      nodeMapList :: [(CfNode (), Int)]
      nodeMapList = zip (HashSet.toList $ G.nodes cflow) [0..]
      
      nodeMap :: HashMap (CfNode ()) Int
      nodeMap = HashMap.fromList nodeMapList

      showNodeMapping :: [Token]
      showNodeMapping = intercalate [tt "\n"] $ showNode <$> nodeMapList

      showNode (node, id) =
        [tt (show id), tt " : "] ++
        (tokenize . fromJust $ G.getNodeAttr node cflow)

      showEdges :: [Token]
      showEdges =
        [ tt
          . Text.concat
          . fmap (cs . pshow)
          . fmap (fmap $ fromJust . flip HashMap.lookup nodeMap)
          . G.edges
          $ cflow
        ]

      showAttrs :: [Token]
      showAttrs = intercalate [tt "\n"] $ mapMaybe showAttr nodeMapList

      showAttr (node, id) = do
        attr <- G.getNodeAttr node cflow
        return $ [tt (show id), tt " : "] ++ tokenizeAsList (toList attr)

prettyStmts :: (MonadIO m, Tokenizable a, HasField' "op" a (Pil.ExprOp a))
            => [Pil.Statement a] -> m ()
prettyStmts = prettyPrint . PStmts

prettyIndexedStmts :: (MonadIO m, Tokenizable a, HasField' "op" a (Pil.ExprOp a))
                   => [(Int, Pil.Statement a)] -> m ()
prettyIndexedStmts = prettyPrint . PIndexedStmts

-- | Tokenizable print to IO.
prettyPrint :: (MonadIO m, Tokenizable a) => a -> m ()
prettyPrint = putText . pretty

pp :: (MonadIO m, Tokenizable a) => a -> m ()
pp = prettyPrint

newtype PrettyShow a = PrettyShow a
  deriving (Eq, Ord, Generic)

instance Tokenizable a => Show (PrettyShow a) where
  show (PrettyShow x) = cs $ pretty x
