{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Use :" -}

module Blaze.Pretty
  ( (<++>),
    PrettyShow(..),
    PrettyShow'(..),
    PStmts(..),
    PIndexedStmts(..),
    TokenType(..),
    TokenContext(..),
    Token(..),
    TokenizerCtx(..),
    Tokenizer(),
    Tokenizable(..),
    runTokenize,
    blankTokenizerCtx,
    plainToken,
    varToken,
    addressToken,
    stringToken,
    textToken,
    keywordToken,
    mkTokenizerCtx,
    paren,
    bracket,
    brace,
    delimitedList,
    tokenizeAsList,
    tokenizeAsTuple,
    tokenizeAsCurlyList,
    pretty,
    pretty',
    prettyPrint,
    prettyPrint',
    prettyStmts,
    prettyStmts',
    prettyIndexedStmts,
    prettyIndexedStmts',
    pp,
    pp',
    showHex,
  )
where

import Prelude (id)

import qualified Binja.Function
import qualified Binja.MLIL as MLIL
import Blaze.Prelude hiding (Symbol, bracket, const, sym)
import qualified Blaze.Types.Function as Func
import qualified Blaze.Types.Pil as Pil
import qualified Prelude (show)

import qualified Blaze.CallGraph as Cg
import qualified Blaze.Graph as G
import qualified Blaze.Types.Pil.Checker as PI
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Numeric

import Blaze.Cfg
    ( CfNode(BasicBlock, Call, EnterFunc, LeaveFunc),
      BasicBlockNode,
      CallNode,
      EnterFuncNode,
      LeaveFuncNode,
      BranchType,
      CfEdge,
      Cfg,
      getCtxIndices
    )
import qualified Blaze.Cfg as Cfg
import qualified Blaze.Types.Cfg.Grouping as GCfg
import Blaze.Pil.Display (needsParens)
import Blaze.Types.Cfg.Interprocedural (InterCfg (InterCfg))
import Blaze.Types.Pil (Ctx)
import qualified Data.Bimap as Bimap
import qualified Data.HashMap.Strict as HashMap
import Data.SBV.Dynamic (SVal)

data TokenType
  = TextToken
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

data TokenContext
  = NoTokenContext
  | LocalVariableTokenContext
  | DataVariableTokenContext
  | FunctionReturnTokenContext
  | InstructionAddressTokenContext
  | ILInstructionIndexTokenContext
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

newtype TokenizerCtx = TokenizerCtx
  { ctxIndices :: Bimap.Bimap Int Ctx
  }
  deriving (Eq, Ord, Show, Generic)

blankTokenizerCtx :: TokenizerCtx
blankTokenizerCtx =
  TokenizerCtx
    { ctxIndices = Bimap.empty
    }

mkTokenizerCtx :: Cfg.PilCfg -> TokenizerCtx
mkTokenizerCtx cfg =
  TokenizerCtx
    { ctxIndices = getCtxIndices cfg
    }

newtype Tokenizer a = Tokenizer
  { runTokenizer :: Reader TokenizerCtx a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader TokenizerCtx
    )

class Collectable a where
  collect :: a -> Tokenizer [Token]

instance Collectable Token where
  collect = pure . (: [])

instance Collectable [Token] where
  collect = pure

instance Collectable (Tokenizer [Token]) where
  collect = id

infixr 5 <++>
(<++>) :: (Collectable a, Collectable b) => a -> b -> Tokenizer [Token]
fxs <++> fys = (<>) <$> collect fxs <*> collect fys

class Tokenizable a where
  tokenize :: a -> Tokenizer [Token]

runTokenize :: Tokenizable a => TokenizerCtx -> a -> [Token]
runTokenize ctx =
  flip runReader ctx
    . runTokenizer
    . tokenize

plainToken :: TokenType -> Text -> Token
plainToken typ t =
  Token
    { tokenType = typ
    , text = t
    , value = 0
    , size = 0
    , operand = 0xffffffff
    , context = NoTokenContext
    , address = 0
    }

integerToken :: Integral n => n -> Token
integerToken n =
  Token
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

instance Tokenizable Int where
  tokenize = pure . (: []) . integerToken

-- XXX Figure out value and operand here
varToken :: Text -> Token
varToken t =
  Token
    { tokenType = LocalVariableToken
    , text = t
    , value = 0
    , size = 0
    , operand = 0
    , context = LocalVariableTokenContext
    , address = 0
    }

addressToken :: Maybe Text -> Address -> Token
addressToken t addr =
  Token
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

between :: [a] -> [a] -> [a] -> [a]
between start end xs = start ++ xs ++ end

paren :: [Token] -> [Token]
paren = between [tt "("] [tt ")"]

bracket :: [Token] -> [Token]
bracket = between [tt "["] [tt "]"]

brace :: [Token] -> [Token]
brace = between [tt "{"] [tt "}"]

delimitedList :: [a] -> [a] -> [a] -> [[a]] -> [a]
delimitedList start sep end = between start end . concat . intersperse sep

tokenizeAsList :: Tokenizable a => [a] -> Tokenizer [Token]
tokenizeAsList x = delimitedList [tt "["] [tt ", "] [tt "]"] <$> traverse tokenize x

tokenizeAsTuple :: Tokenizable a => [a] -> Tokenizer [Token]
tokenizeAsTuple x = delimitedList [tt "("] [tt ", "] [tt ")"] <$> traverse tokenize x

tokenizeAsCurlyList :: Tokenizable a => [a] -> Tokenizer [Token]
tokenizeAsCurlyList x = delimitedList [tt "{"] [tt ", "] [tt "}"] <$> traverse tokenize x

pretty :: Tokenizable a => TokenizerCtx -> a -> Text
pretty ctx = foldMap (view #text) . runTokenize ctx

pretty' :: Tokenizable a => a -> Text
pretty' = pretty blankTokenizerCtx

showHex :: (Integral a, Show a) => a -> Text
showHex x
  | x < 0 = Text.pack $ "-0x" <> Numeric.showHex (abs x) ""
  | otherwise = Text.pack $ "0x" <> Numeric.showHex x ""

showStackLocalByteOffset :: ByteOffset -> Text
showStackLocalByteOffset x =
  bool "arg_" "var_" (x < 0)
    <> (Text.pack . flip Numeric.showHex "" . abs $ x)

parenExpr :: (Tokenizable a, HasField' "op" a (Pil.ExprOp a)) => a -> Tokenizer [Token]
parenExpr x =
  if needsParens $ x ^. #op
    then paren <$> tokenize x
    else tokenize x

instance Tokenizable a => Tokenizable [a] where
  tokenize = tokenizeAsList

instance Tokenizable () where
  tokenize () = pure [tt "()"]

instance Tokenizable Token where
  tokenize t = pure [t]

instance (Tokenizable k, Tokenizable v) => Tokenizable (Map k v) where
  tokenize m = keywordToken "Map: " <++> tokenizeAsList (Map.toList m)

instance Tokenizable a => Tokenizable (HashSet a) where
  tokenize x = delimitedList [tt "#{"] [tt ", "] [tt "}"] <$> traverse tokenize (HashSet.toList x)

instance Tokenizable (MLIL.Expression a) where
  tokenize _ = pure [tt "(TODO: MLIL Expression)"]

instance (Tokenizable a, Tokenizable b) => Tokenizable (a, b) where
  tokenize (a, b) = paren <$> tokenize a <++> tt ", " <++> tokenize b

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
    pure
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
    [keywordToken "ctx", tt " "]
      <++> (paren <$> tokenize (ctx ^. #func))
      <++> tt " "
      <++> textToken (show $ ctx ^. #ctxId)

instance Tokenizable Pil.PilVar where
  tokenize var = do
    ctxIndices' <- view #ctxIndices
    let ctxIdSuff =
          either id id $ do
            if Bimap.size ctxIndices' > 1 then Right () else Left ""
            ctx <- maybeToEither "@?" (var ^. #ctx)
            ctxIdIndex <- maybeToEither "@?" $ Bimap.lookupR ctx ctxIndices'
            pure $ "@" <> show ctxIdIndex
    pure [varToken $ (var ^. #symbol) <> ctxIdSuff]

tokenizeBinop ::
  ( Tokenizable b
  , HasField' "left" a b
  , HasField' "right" a b
  , HasField' "op" b (Pil.ExprOp b)
  ) =>
  Text ->
  a ->
  Tokenizer [Token]
tokenizeBinop sym op =
  tt (sym <> " ")
    <++> parenExpr (op ^. #left)
    <++> tt " "
    <++> parenExpr (op ^. #right)

tokenizeBinopInfix ::
  ( Tokenizable b
  , HasField' "left" a b
  , HasField' "right" a b
  , HasField' "op" b (Pil.ExprOp b)
  ) =>
  Text ->
  a ->
  Tokenizer [Token]
tokenizeBinopInfix sym op =
  parenExpr (op ^. #left)
    <++> tt (" " <> sym <> " ")
    <++> parenExpr (op ^. #right)

tokenizeUnop ::
  ( HasField' "src" a b
  , Tokenizable b
  , HasField' "op" b (Pil.ExprOp b)
  ) =>
  Text ->
  a ->
  Tokenizer [Token]
tokenizeUnop sym op = tt (sym <> " ") <++> parenExpr (op ^. #src)

tokenizeField ::
  ( HasField' "src" a b
  , Tokenizable b
  , HasField' "offset" a ByteOffset
  ) =>
  a ->
  Tokenizer [Token]
tokenizeField op =
  tokenize (op ^. #src)
    <++> (bracket <$> tokenize (op ^. #offset))

tokenizeExprOp ::
  (Tokenizable a, HasField' "op" a (Pil.ExprOp a)) =>
  Pil.ExprOp a ->
  Pil.OperationSize ->
  Tokenizer [Token]
tokenizeExprOp exprOp _size = case exprOp of
  (Pil.ADC op) -> tokenizeBinop "adc" op
  (Pil.ADD op) -> tokenizeBinopInfix "+" op
  (Pil.ADD_OVERFLOW op) -> tokenizeBinopInfix "&+" op
  (Pil.AND op) -> tokenizeBinopInfix "&&" op
  (Pil.ASR op) -> tokenizeBinop "asr" op
  (Pil.BOOL_TO_INT op) -> tokenizeUnop "boolToInt" op
  (Pil.CEIL op) -> tokenizeUnop "ceil" op
  (Pil.CONST_BOOL op) -> pure [tt . show $ op ^. #constant]
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
  (Pil.CONST op) -> pure [integerToken (op ^. #constant)]
  (Pil.CONST_PTR op) -> pure [addressToken Nothing $ fromIntegral (op ^. #constant)]
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
    parenExpr (op ^. #baseAddr)
      <++> [tt " + ", keywordToken "offset", tt " "]
      <++> tokenize (op ^. #offset)
  (Pil.CONST_FLOAT op) -> pure [plainToken FloatingPointToken $ show (op ^. #constant)]
  (Pil.FLOAT_CONV op) -> tokenizeUnop "floatConv" op
  (Pil.FLOAT_TO_INT op) -> tokenizeUnop "floatToInt" op
  (Pil.FLOOR op) -> tokenizeUnop "floor" op
  (Pil.FMUL op) -> tokenizeBinopInfix "f*" op
  (Pil.FNEG op) -> tokenizeUnop "fneg" op
  (Pil.FSQRT op) -> tokenizeUnop "fsqrt" op
  (Pil.FSUB op) -> tokenizeBinopInfix "f-" op
  (Pil.FTRUNC op) -> tokenizeUnop "ftrunc" op
  (Pil.IMPORT op) -> pure [addressToken Nothing $ fromIntegral (op ^. #constant)]
  (Pil.INT_TO_FLOAT op) -> tokenizeUnop "intToFloat" op
  (Pil.LOAD op) -> bracket <$> tokenize (op ^. #src)
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
    pure
      [ tt "&"
      , varToken $ showStackLocalByteOffset (op ^. #stackOffset . #offset)
      ]
  (Pil.SUB op) -> tokenizeBinopInfix "-" op
  (Pil.SX op) -> tokenizeUnop "sx" op
  (Pil.TEST_BIT op) -> tokenizeBinop "testBit" op
  (Pil.UNIMPL t) -> keywordToken "unimpl" <++> paren [tt t]
  (Pil.UPDATE_VAR op) ->
    keywordToken "updateVar" <++> (paren <$> parts)
    where
      arg name val more = [plainToken ArgumentNameToken name, tt ": "] <++> val <++> [tt ", " | more]
      parts =
        arg "var" [varToken $ op ^. #dest . #symbol] True
          <++> arg "offset" (tokenize $ op ^. #offset) True
          <++> arg "val" (tokenize $ op ^. #src) False
  (Pil.VAR_PHI op) -> [tt (op ^. #dest ^. #symbol), tt " <- "] <++> srcs
    where
      srcs :: [Token]
      srcs = tt . view #symbol <$> (op ^. #src)
  (Pil.VAR_JOIN op) ->
    tt "varJoin "
      <++> tokenize (op ^. #high)
      <++> tt " "
      <++> tokenize (op ^. #low)
  -- TODO: Need added
  (Pil.VAR op) -> tokenize (op ^. #src)
  -- TODO: Add field offset
  (Pil.VAR_FIELD op) -> tokenizeField op
  (Pil.XOR op) -> tokenizeBinop "xor" op
  (Pil.ZX op) -> tokenizeUnop "zx" op
  (Pil.CALL op) -> tokenize op
  (Pil.StrCmp op) -> tokenizeBinop "strcmp" op
  (Pil.StrNCmp op) ->
    [tt "strncmp ", integerToken (op ^. #len), tt " "]
      <++> tokenize (op ^. #left)
      <++> tt " "
      <++> tokenize (op ^. #right)
  (Pil.MemCmp op) -> tokenizeBinop "memcmp" op
  (Pil.ExternPtr op) -> tokenize op
  -- TODO: Should ConstStr also use const rather than value as field name?
  (Pil.ConstStr op) -> pure [stringToken $ op ^. #value]
  (Pil.ConstFuncPtr op) -> tokenize op
  (Pil.Extract op) ->
    [keywordToken "extract", tt " "]
      <++> tokenize (op ^. #src)
      <++> tt " "
      <++> tokenize (op ^. #offset)
  Pil.UNIT -> pure [tt "()"]

instance Tokenizable PI.Constraint where
  tokenize (PI.Constraint stmtIndex s stype) =
    [tt $ show stmtIndex, tt ":"]
      <++> tokenize s
      <++> [tt ": "]
      <++> tokenize stype

instance Tokenizable PI.SymType where
  tokenize (PI.SVar s) = tokenize s
  tokenize (PI.SType t) = tokenize t

instance Tokenizable PI.DeepSymType where
  tokenize (PI.DSVar s) = tokenize s
  tokenize (PI.DSType t) = tokenize t
  tokenize (PI.DSRecursive s dst) =
    paren
      <$> ( [keywordToken "Rec", tt " "]
              <++> tokenize s
              <++> tt ": "
              <++> tokenize dst
          )

instance Tokenizable PI.Sym where
  tokenize (PI.Sym n) = pure [tt $ "s" <> show n]

instance Tokenizable (PI.InfoExpression (PI.SymInfo, Maybe PI.SymType)) where
  tokenize (PI.InfoExpression (PI.SymInfo bitwidth s, mstype) op) =
    tokenizeExprOp op (coerce $ bitwidth * 8)
      <++> tt " :: "
      <++> fmap paren (tokenize s <++> tt " | " <++> mstype')
    where
      mstype' = maybe (pure [keywordToken "Unknown"]) tokenize mstype

instance Tokenizable (PI.InfoExpression (PI.SymInfo, Maybe PI.DeepSymType)) where
  tokenize (PI.InfoExpression (PI.SymInfo bitwidth s, mstype) op) =
    tokenizeExprOp op (coerce $ bitwidth * 8)
      <++> tt " :: "
      <++> (paren <$> tokenize s <++> tt " | " <++> mstype')
    where
      mstype' = maybe (pure [keywordToken "Unknown"]) tokenize mstype

instance Tokenizable (PI.InfoExpression PI.SymInfo) where
  tokenize (PI.InfoExpression (PI.SymInfo bitwidth (PI.Sym n)) op) =
    [tt (show n), tt ":"]
      <++> (paren <$> tokenizeExprOp op (coerce $ bitwidth * 8))

instance Tokenizable Pil.Expression where
  tokenize (Pil.Expression size' exprOp) = tokenizeExprOp exprOp size'

instance (Tokenizable a, Tokenizable b) => Tokenizable (HashMap a b) where
  tokenize m =
    [keywordToken "HashMap:", tt "\n"]
      <++> (intercalate [tt "\n"] <$> traverse f (HashMap.toList m))
    where
      f pair = tt "  " <++> tokenize pair

instance (Tokenizable a, HasField' "op" a (Pil.ExprOp a)) => Tokenizable (Pil.Statement a) where
  tokenize stmt = case stmt of
    Pil.Def x ->
      tokenize (x ^. #var)
        <++> tt " = "
        <++> tokenize (x ^. #value)
    Pil.Constraint x -> tt "?: " <++> tokenize (x ^. #condition)
    Pil.Store x ->
      (bracket <$> tokenize (x ^. #addr))
        <++> tt " = "
        <++> tokenize (x ^. #value)
    Pil.UnimplInstr t ->
      let shortRepr = show $ Text.take 10 t
       in pure
            [ tt "Unimplemented Instruction (\""
            , tt (Text.dropEnd 1 . Text.drop 1 $ shortRepr)
            , tt "..."
            , tt "\")"
            ]
    Pil.UnimplMem x -> tt "Unimplemented Memory: " <++> (bracket <$> tokenize (x ^. #src))
    Pil.Undef -> pure [keywordToken "Undefined"]
    Pil.Nop -> pure [keywordToken "Nop"]
    Pil.Annotation t -> pure [plainToken CommentToken "// ", plainToken CommentToken t]
    Pil.EnterContext x -> tt "----> Entering " <++> tokenize (x ^. #ctx)
    Pil.ExitContext x -> tt "<---- Leaving " <++> tokenize (x ^. #leavingCtx)
    Pil.Call callOp -> tokenize callOp
    Pil.DefPhi x ->
      tokenize (x ^. #dest)
        <++> [tt " = ", keywordToken "φ"]
        <++> tokenizeAsCurlyList (x ^. #src)
    Pil.DefMemPhi x ->
      varToken ("mem#" <> show (x ^. #destMemory))
        <++> [tt " = ", tt "φ"]
        <++> tokenizeAsCurlyList ((\m -> varToken $ "mem#" <> show m) <$> (x ^. #srcMemory))
    Pil.BranchCond x -> [keywordToken "if", tt " "] <++> (paren <$> tokenize (x ^. #cond))
    Pil.Jump x -> [keywordToken "jump", tt " "] <++> parenExpr (x ^. #dest)
    Pil.JumpTo x ->
      parenExpr (x ^. #dest)
        <++> [keywordToken "jumpTo", tt " "]
        <++> tokenizeAsList (x ^. #targets)
    Pil.Ret x -> [keywordToken "return", tt " "] <++> tokenize (x ^. #value)
    Pil.NoRet -> pure [keywordToken "NoRet"]
    Pil.Exit -> pure [keywordToken "Exit"]
    Pil.TailCall x ->
      [keywordToken "Tail call", tt " | "]
        <++> tokenize (x ^. #dest)
        <++> tokenizeAsList (x ^. #args)

instance Tokenizable a => Tokenizable (Pil.CallOp a) where
  tokenize op =
    case (op ^. #name, op ^. #dest) of
      (Just name, Pil.CallAddr fptr) ->
        addressToken (Just name) (fptr ^. #address)
          <++> tokenizeAsTuple (op ^. #params)
      _ ->
        [keywordToken "call", tt " "]
          <++> tokenize (op ^. #dest)
          <++> tokenizeAsTuple (op ^. #params)

instance Tokenizable Pil.CallSite where
  tokenize x =
    tokenize (x ^. #caller)
      <++> tt " -> "
      <++> tokenize (x ^. #callDest)

newtype PStmts a = PStmts [Pil.Statement a]

newtype PIndexedStmts a = PIndexedStmts [(Int, Pil.Statement a)]

instance (Tokenizable a, HasField' "op" a (Pil.ExprOp a)) => Tokenizable (PStmts a) where
  tokenize (PStmts stmts) = intercalate [tt "\n"] <$> traverse tokenize stmts

instance (Tokenizable a, HasField' "op" a (Pil.ExprOp a)) => Tokenizable (PIndexedStmts a) where
  tokenize (PIndexedStmts stmts) = intercalate [tt "\n"] <$> traverse f stmts
    where
      f :: (Int, Pil.Statement a) -> Tokenizer [Token]
      f (i, stmt) = [integerToken i, tt ": "] <++> tokenize stmt

instance Tokenizable ByteOffset where
  tokenize (ByteOffset n) = pure [integerToken n]

instance Tokenizable Address where
  tokenize (Address n) = pure [integerToken n]

instance Tokenizable Int64 where
  tokenize n = pure [integerToken n]

instance Tokenizable Pil.StackOffset where
  tokenize x =
    tt "stackOffset "
      <++> tokenize (x ^. #offset)
      <++> (paren <$> tokenize (x ^. #ctx))

instance Tokenizable Pil.StmtIndex where
  tokenize x = pure [integerToken (x ^. #val)]

instance Tokenizable t => Tokenizable (PI.PilType t) where
  tokenize = \case
    PI.TArray len elemType ->
      [keywordToken "Array", tt " "]
        <++> tokenize len
        <++> tt " "
        <++> tokenize elemType
    --    PI.TZeroField pt -> "ZeroField" <-> paren (tokenize pt)
    PI.TBool -> pure [keywordToken "Bool"]
    PI.TChar -> pure [keywordToken "Char"]
    -- PI.TQueryChar -> "QueryChar"
    PI.TInt bitWidth signed ->
      [keywordToken "Int", tt " "]
        <++> tokenize bitWidth
        <++> tt " "
        <++> tokenize signed
    PI.TFloat bitWidth -> [keywordToken "Float", tt " "] <++> tokenize bitWidth
    PI.TBitVector bitWidth -> [keywordToken "BitVector", tt " "] <++> tokenize bitWidth
    PI.TPointer bitWidth pointeeType ->
      [keywordToken "Pointer", tt " "]
        <++> tokenize bitWidth
        <++> tt " "
        <++> (paren <$> tokenize pointeeType)
    PI.TCString len -> [keywordToken "CString", tt " "] <++> tokenize len
    PI.TRecord m ->
      [keywordToken "Record", tt " "]
        <++> ( delimitedList [tt "["] [tt ", "] [tt "]"]
                <$> traverse rfield (HashMap.toList m)
             )
      where
        rfield :: forall a. Tokenizable a => (BitOffset, a) -> Tokenizer [Token]
        rfield (BitOffset n, t) = paren <$> [tt (show n), tt ", "] <++> tokenize t
    PI.TBottom s -> paren <$> [keywordToken "Bottom", tt " "] <++> tokenize s
    PI.TUnit -> pure [keywordToken "Unit"]
    PI.TFunction _ret _params -> pure [keywordToken "Func"]
    PI.TVBitWidth (Bits bitWidth) -> pure [tt $ show bitWidth <> "w"]
    PI.TVLength n -> pure [integerToken n]
    PI.TVSign b -> pure [keywordToken (if b then "Signed" else "Unsigned")]

--- CallGraph
instance Tokenizable Cg.CallDest where
  tokenize (Cg.DestFunc x) = tokenize x

instance Tokenizable Cg.CallSite where
  tokenize x =
    tokenize (x ^. #caller)
      <++> tt " -> "
      <++> tokenize (x ^. #dest)

--- Function
instance Tokenizable Func.Function where
  tokenize (Func.Function _ name addr _) =
    pure
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
  tokenize x = pure [addressToken (x ^. #symbol) (x ^. #address)]

instance Tokenizable Pil.ExternPtrOp where
  tokenize x = [tt "extern ", addressToken (x ^. #symbol) addr] <++> moff
    where
      moff = case x ^. #offset of
        0 -> []
        n -> bracket [integerToken n]
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
    tokenize (e ^. #src)
      <++> tt " ---> "
      <++> tokenize (e ^. #dst)
      <++> tt "  |"
      <++> tokenize (e ^. #branchType)
      <++> tt "|"

instance Tokenizable (BasicBlockNode a) where
  tokenize (Cfg.BasicBlockNode ctx start end _ _) =
    tokenize ctx
      <++> [ tt "@["
           , addressToken Nothing start
           , tt ", "
           , addressToken Nothing end
           , tt "]"
           ]

instance Tokenizable (GCfg.GroupingNode a) where
  tokenize (GCfg.GroupingNode _termNode _uuid' _grouping) =
    -- TODO: Improve
    tokenize [ tt "Grouping" ]

instance Tokenizable (CallNode a) where
  tokenize (Cfg.CallNode ctx start dest _ _) =
    tokenize ctx
      <++> [tt "@", addressToken Nothing start, tt " -> "]
      <++> tokenize dest

instance Tokenizable (EnterFuncNode a) where
  tokenize (Cfg.EnterFuncNode prevCtx nextCtx _ _) =
    tt "EnterFunc Ctx: "
      <++> tokenize prevCtx
      <++> tt " -> "
      <++> tokenize nextCtx

instance Tokenizable (LeaveFuncNode a) where
  tokenize (Cfg.LeaveFuncNode prevCtx nextCtx _ _) =
    tt "LeaveFunc Ctx: "
      <++> tokenize prevCtx
      <++> tt " -> "
      <++> tokenize nextCtx

instance Tokenizable BranchType where
  tokenize bt = pure [tt (show bt)]


instance Tokenizable (GCfg.CfNode a) where
  tokenize = \case
    GCfg.BasicBlock n -> tokenize n
    GCfg.Call n -> tokenize n
    GCfg.EnterFunc n -> tokenize n
    GCfg.LeaveFunc n -> tokenize n
    GCfg.Grouping n -> tokenize n

instance Tokenizable (GCfg.CfEdge a) where
  tokenize e =
    tokenize (e ^. #src) <++>
    tt " ---> " <++>
    tokenize (e ^. #dst) <++>
    tt "  |" <++>
    tokenize (e ^. #branchType) <++>
    tt "|"

  

-- instance Tokenizable [Pil.Stmt] where
--   tokenize = intercalate [tt "\n"] . fmap tokenize

instance Tokenizable InterCfg where
  tokenize (InterCfg cfg) = tokenize cfg

-- | This matches each node to an Int and uses the Int to show the edges
instance Tokenizable a => Tokenizable (Cfg a) where
  tokenize cfg =
    [tt "---CFG---\n", tt "--- Node Mapping:\n"]
      <++> showNodeMapping
      <++> tt "--- Edges:\n"
      <++> showEdges
      <++> tt "--- Attrs:\n"
      <++> showAttrs
    where
      cflow = cfg ^. #graph

      nodeMapList :: [(CfNode (), Int)]
      nodeMapList = zip (HashSet.toList $ G.nodes cflow) [0 ..]

      nodeMap :: HashMap (CfNode ()) Int
      nodeMap = HashMap.fromList nodeMapList

      showNodeMapping :: Tokenizer [Token]
      showNodeMapping = intercalate [tt "\n"] <$> traverse showNode nodeMapList

      showNode (node, nid) =
        [tt (show nid), tt " : "]
          <++> (tokenize . fromJust $ G.getNodeAttr node cflow)

      showEdges :: Tokenizer [Token]
      showEdges =
        pure
          [ tt
              . Text.concat
              . fmap (cs . pshow)
              . fmap (fmap $ fromJust . flip HashMap.lookup nodeMap)
              . G.edges
              $ cflow
          ]

      showAttrs :: Tokenizer [Token]
      showAttrs = intercalate [tt "\n"] <$> sequence (mapMaybe showAttr nodeMapList)

      showAttr :: forall t. Show t => (CfNode (), t) -> Maybe (Tokenizer [Token])
      showAttr (node, nid) = do
        attr <- G.getNodeAttr node cflow
        return $ [tt (show nid), tt " : "] <++> tokenizeAsList (toList attr)

-- | Tokenizable print to IO.
prettyPrint :: (MonadIO m, Tokenizable a) => TokenizerCtx -> a -> m ()
prettyPrint ctx = putText . pretty ctx

prettyPrint' :: (MonadIO m, Tokenizable a) => a -> m ()
prettyPrint' = prettyPrint blankTokenizerCtx

pp :: (MonadIO m, Tokenizable a) => TokenizerCtx -> a -> m ()
pp = prettyPrint

pp' :: (MonadIO m, Tokenizable a) => a -> m ()
pp' = prettyPrint'

prettyStmts :: (MonadIO m, Tokenizable a, HasField' "op" a (Pil.ExprOp a)) => TokenizerCtx -> [Pil.Statement a] -> m ()
prettyStmts ctx = prettyPrint ctx . PStmts

prettyStmts' :: (MonadIO m, Tokenizable a, HasField' "op" a (Pil.ExprOp a)) => [Pil.Statement a] -> m ()
prettyStmts' = prettyPrint' . PStmts

prettyIndexedStmts :: (MonadIO m, Tokenizable a, HasField' "op" a (Pil.ExprOp a)) => TokenizerCtx -> [(Int, Pil.Statement a)] -> m ()
prettyIndexedStmts ctx = prettyPrint ctx . PIndexedStmts

prettyIndexedStmts' :: (MonadIO m, Tokenizable a, HasField' "op" a (Pil.ExprOp a)) => [(Int, Pil.Statement a)] -> m ()
prettyIndexedStmts' = prettyPrint' . PIndexedStmts

instance Tokenizable SVal where
  tokenize x = pure [tt $ show x]

instance Tokenizable a => Tokenizable (Maybe a) where
  tokenize Nothing = pure [tt "Nothing"]
  tokenize (Just x) = [tt "Just"] <++> tokenize x

instance Tokenizable a => Tokenizable (G.Dominators a) where
  tokenize (G.Dominators m) = tokenize m

instance Tokenizable a => Tokenizable (G.PostDominators a) where
  tokenize (G.PostDominators m) = tokenize m

instance Tokenizable Bool where
  tokenize b = pure [tt $ show b]

instance Tokenizable Text where
  tokenize t = pure [tt t]

instance Tokenizable a => Tokenizable (GCfg.Cfg a) where
  tokenize cfg =
    [tt "---CFG---\n", tt "--- Node Mapping:\n"] <++>
    showNodeMapping <++>
    tt "--- Edges:\n" <++>
    showEdges <++>
    tt "--- Attrs:\n" <++>
    showAttrs
    where
      cflow = cfg ^. #graph
      nodeMapList :: [(GCfg.CfNode (), Int)]
      nodeMapList = zip (HashSet.toList $ G.nodes cflow) [0..]

      nodeMap :: HashMap (GCfg.CfNode ()) Int
      nodeMap = HashMap.fromList nodeMapList

      showNodeMapping :: Tokenizer [Token]
      showNodeMapping = intercalate [tt "\n"] <$> traverse showNode nodeMapList

      showNode :: (GCfg.CfNode (), Int) -> Tokenizer [Token]
      showNode (node, id) =
        [tt (show id), tt " : "] <++>
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

      showAttrs :: Tokenizer [Token]
      showAttrs = intercalate [tt "\n"] <$> sequence (mapMaybe showAttr nodeMapList)

      showAttr :: (GCfg.CfNode (), Int) -> Maybe (Tokenizer [Token])
      showAttr (node, id) = do
        attr <- G.getNodeAttr node cflow
        return $ [tt (show id), tt " : "] <++> tokenizeAsList (toList attr)

instance Tokenizable GCfg.GroupSpec where
  tokenize gs =
    [keywordToken "GroupSpec"] <++>
    ( paren <$>
      ( arg "root" <++>
        tokenize (gs ^. #groupRoot) <++>
        [tt ", "] <++>
        arg "end" <++>
        tokenize (gs ^. #groupTerm) <++>
        [tt ", "] <++>
        arg "inner" <++>
        tokenize (gs ^. #innerGroups)
      ))
    where
      arg :: Text -> Tokenizer [Token]
      arg a = pure [plainToken ArgumentNameToken a, tt " = "]

data PrettyShow a = PrettyShow TokenizerCtx a
  deriving (Eq, Ord, Generic)

instance Tokenizable a => Show (PrettyShow a) where
  show (PrettyShow ctx x) = cs $ pretty ctx x

newtype PrettyShow' a = PrettyShow' a
  deriving (Eq, Ord, Generic)

instance Tokenizable a => Show (PrettyShow' a) where
  show (PrettyShow' x) = cs $ pretty' x
