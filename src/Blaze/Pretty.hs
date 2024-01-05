{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Use :" -}

module Blaze.Pretty
  ( (<++>),
    FullCfNode(FullCfNode),
    NewlinedList(NewlinedList),
    PrettyShow(..),
    PrettyShow'(..),
    PStmts(..),
    PIndexedStmts(..),
    TokenType(..),
    TokenContext(..),
    Token(..),
    TokenizerCtx,
    Tokenizer(),
    Tokenizable(..),
    runTokenize,
    blankTokenizerCtx,
    instructionToken,
    instr,
    plainToken,
    varToken,
    addressToken,
    stringToken,
    textToken,
    tt,
    keywordToken,
    kt,
    mkTokenizerCtx,
    paren,
    bracket,
    brace,
    delimitedList,
    tokenizeAsList,
    tokenizeAsTuple,
    tokenizeAsCurlyList,
    tokenizeExprOp,
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
import qualified Prelude (show)
import qualified Binja.Function
import qualified Binja.MLIL as MLIL
import Blaze.Prelude hiding (Symbol, bracket, const, sym)
import qualified Blaze.Types.Function as Func
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil (PilVar)

import qualified Blaze.CallGraph as Cg
import qualified Blaze.Graph as G
import Blaze.Types.Path.Alga (AlgaPath)
import qualified Blaze.Types.Path as Path
import Blaze.Types.Pil.Analysis (LoadExpr(LoadExpr))
import qualified Blaze.Types.Pil.Checker as PI
import Blaze.Types.Pil.Checker (Sym)
import Blaze.Types.Pil.Summary (Effect (EffectWrite, EffectAlloc, EffectDealloc, EffectCall))
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Numeric

import Blaze.Types.Cfg
    ( CfNode(BasicBlock, Call, EnterFunc, LeaveFunc, Grouping),
      BasicBlockNode,
      CallNode,
      EnterFuncNode,
      LeaveFuncNode,
      GroupingNode (GroupingNode),
      BranchType,
      CfEdge,
      Cfg,
    )
import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Types.Cfg.Grouping as GCfg
import qualified Blaze.Types.Cfg.Path as CfgPath
import qualified Blaze.Types.Pil.Summary as Summary
import Blaze.Pil.Display (needsParens, NeedsParens)

import qualified Data.HashMap.Strict as HashMap
import Data.SBV.Dynamic (SVal, CV)


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
  deriving (Eq, Ord, Show, Enum, Generic, FromJSON, ToJSON, Hashable)

data TokenContext
  = NoTokenContext
  | LocalVariableTokenContext
  | DataVariableTokenContext
  | FunctionReturnTokenContext
  | InstructionAddressTokenContext
  | ILInstructionIndexTokenContext
  deriving (Eq, Ord, Show, Enum, Generic, FromJSON, ToJSON, Hashable)

data Token = Token
  { tokenType :: TokenType
  , text :: Text
  , value :: Integer
  , size :: Int
  , operand :: Integer
  , context :: TokenContext
  -- , confidence :: Int
  , address :: Address
  , typeSym :: Maybe PI.Sym -- not in Binja's Token type
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, Hashable)

newtype TokenizerCtx = TokenizerCtx
  { varSymMap :: Maybe (HashMap Pil.PilVar PI.Sym)
  } deriving (Eq, Ord, Show, Generic)

blankTokenizerCtx :: TokenizerCtx
blankTokenizerCtx = TokenizerCtx Nothing

mkTokenizerCtx :: Maybe PI.VarSymMap -> TokenizerCtx
mkTokenizerCtx = TokenizerCtx

newtype Tokenizer a = Tokenizer
  { runTokenizer :: Reader TokenizerCtx a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader TokenizerCtx
    )

setSym :: Maybe Sym -> Token -> Token
setSym s t = t & #typeSym .~ s

getVarSym :: PilVar -> Tokenizer (Maybe Sym)
getVarSym pv = do
  mvsm <- view #varSymMap
  case mvsm of
    Nothing -> return Nothing
    Just vsm -> return $ HashMap.lookup pv vsm

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
    , typeSym = Nothing
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
    , typeSym = Nothing
    }
  where
    n' = toInteger n

decimalToken :: Integral n => n -> Token
decimalToken n =
  Token
    { tokenType = IntegerToken
    , text = show n'
    , value = n'
    , size = 0
    , operand = 0xffffffff
    , context = NoTokenContext
    , address = 0
    , typeSym = Nothing
    }
  where
    n' = toInteger n

instance Tokenizable Int where
  tokenize = pure . (: []) . integerToken

-- XXX Figure out value and operand here
varToken :: Maybe Sym -> Text -> Token
varToken mTypeSym t =
  Token
    { tokenType = LocalVariableToken
    , text = t
    , value = 0
    , size = 0
    , operand = 0
    , context = LocalVariableTokenContext
    , address = 0
    , typeSym = mTypeSym
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
    , typeSym = Nothing
    }

stringToken :: Text -> Token
stringToken = plainToken StringToken . show

textToken :: Text -> Token
textToken = plainToken TextToken

tt :: Text -> Token
tt = textToken

instructionToken :: Text -> Token
instructionToken = plainToken InstructionToken

instr :: Text -> Token
instr = instructionToken

keywordToken :: Text -> Token
keywordToken = plainToken KeywordToken

kt :: Text -> Token
kt = keywordToken

between :: [a] -> [a] -> [a] -> [a]
between start end xs = start ++ xs ++ end

paren :: [Token] -> [Token]
paren = between [tt "("] [tt ")"]

bracket :: [Token] -> [Token]
bracket = between [tt "["] [tt "]"]

brace :: [Token] -> [Token]
brace = between [tt "{"] [tt "}"]

delimitedList :: [a] -> [a] -> [a] -> [[a]] -> [a]
delimitedList start sep end = between start end . intercalate sep

tokenizeAsList :: Tokenizable a => [a] -> Tokenizer [Token]
tokenizeAsList x = delimitedList [tt "["] [tt ", "] [tt "]"] <$> traverse tokenize x

tokenizeAsNewlinedList :: Tokenizable a => [a] -> Tokenizer [Token]
tokenizeAsNewlinedList [] = return [tt "[]"]
tokenizeAsNewlinedList x = delimitedList [tt "[ "] [tt "\n, "] [tt "\n]"] <$> traverse tokenize x

tokenizeAsTuple :: Tokenizable a => [a] -> Tokenizer [Token]
tokenizeAsTuple x = delimitedList [tt "("] [tt ", "] [tt ")"] <$> traverse tokenize x

tokenizeAsCurlyList :: Tokenizable a => [a] -> Tokenizer [Token]
tokenizeAsCurlyList x = delimitedList [tt "{"] [tt ", "] [tt "}"] <$> traverse tokenize x

pretty :: Tokenizable a => TokenizerCtx -> a -> Text
pretty ctx = foldMap (view #text) . runTokenize ctx

pretty' :: Tokenizable a => a -> Text
pretty' = pretty blankTokenizerCtx

showHex :: (Integral a) => a -> Text
showHex x = Text.pack $ "0x" <> Numeric.showHex (fromIntegral x :: Word64) ""

showStackLocalByteOffset :: ByteOffset -> Text
showStackLocalByteOffset x =
  bool "arg_" "var_" (x < 0)
    <> (Text.pack . flip Numeric.showHex "" . abs $ x)

parenExpr :: (Tokenizable a, NeedsParens a) => a -> Tokenizer [Token]
parenExpr x =
  if needsParens x
    then paren <$> tokenize x
    else tokenize x

instance Tokenizable a => Tokenizable [a] where
  tokenize = tokenizeAsList

newtype NewlinedList a = NewlinedList [a]

instance Tokenizable a => Tokenizable (NewlinedList a) where
  tokenize (NewlinedList xs) = tokenizeAsNewlinedList xs

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
    Pil.CallAddr fptr -> tokenize fptr
    Pil.CallExpr e -> tokenize e
    Pil.CallExprs es -> tokenizeAsList es
    Pil.CallFunc fn -> tokenize fn
    Pil.CallExtern x -> tokenize x
    Pil.CallUnk -> return [tt "unknown"]


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
          , typeSym = Nothing
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
    vsym <- getVarSym var
    let ctxIdSuff = case var ^. #ctx of
          Nothing -> ""
          Just ctx -> "@"
            <> show (fromIntegral $ ctx ^. #ctxId :: Int)
    pure [varToken vsym $ (var ^. #symbol) <> ctxIdSuff]

instance Tokenizable Summary.InputLocation where
  tokenize (Summary.ConcreteInputLocation a) = kt "ConcreteInputLocation " <++> tokenize a
  tokenize (Summary.SymbolicInputLocation v) = kt "SymbolicInputLocation " <++> tokenize v
  tokenize (Summary.PureExpression e) = kt "PureExpression " <++> parenExpr e

instance Tokenizable Summary.OutputLocation where
  tokenize (Summary.ConcreteOutputLocation a) = kt "ConcreteOutputLocation " <++> tokenize a
  tokenize (Summary.SymbolicOutputLocation v) = kt "SymbolicOutputLocation " <++> tokenize v
  tokenize Summary.Returned = pure [kt "Returned"]

instance Tokenizable Summary.Capability where
  tokenize (Summary.CopyCapability to_ from_) =
    kt "Copy " <++> (paren <$> tokenize to_) <++> tt " " <++> (paren <$> tokenize from_)
  tokenize Summary.AddressLeak = pure [kt "AddressLeak"]

tokenizeBinop ::
  ( Tokenizable b
  , HasField' "left" a b
  , HasField' "right" a b
  , NeedsParens b
  ) =>
  Maybe Sym ->
  Text ->
  a ->
  Tokenizer [Token]
tokenizeBinop tsym opSym op =
  setSym tsym (instr opSym)
    <++> tt " "
    <++> parenExpr (op ^. #left)
    <++> tt " "
    <++> parenExpr (op ^. #right)

tokenizeBinopInfix ::
  ( Tokenizable b
  , HasField' "left" a b
  , HasField' "right" a b
  , NeedsParens b
  ) =>
  Maybe Sym ->
  Text ->
  a ->
  Tokenizer [Token]
tokenizeBinopInfix tsym opSym op =
  parenExpr (op ^. #left)
    <++> tt " "
    <++> setSym tsym (instr opSym)
    <++> tt " "
    <++> parenExpr (op ^. #right)

tokenizeUnop ::
  ( HasField' "src" a b
  , Tokenizable b
  , NeedsParens b
  ) =>
  Maybe Sym ->
  Text ->
  a ->
  Tokenizer [Token]
tokenizeUnop tsym opSym op = setSym tsym (instr opSym)
  <++> tt " "
  <++> parenExpr (op ^. #src)

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
  (Tokenizable a, NeedsParens a) =>
  Maybe Sym ->
  Pil.ExprOp a ->
  Pil.Size a ->
  Tokenizer [Token]
tokenizeExprOp msym exprOp _size = case exprOp of
  (Pil.ADC op) -> tokenizeBinop msym "adc" op
  (Pil.ADD op) -> tokenizeBinopInfix msym "+" op
  (Pil.ADD_WILL_CARRY op) -> tokenizeBinop msym "addOF" op
  (Pil.ADD_WILL_OVERFLOW op) -> tokenizeBinop msym "addCF" op
  (Pil.AND op) -> tokenizeBinopInfix msym "&&" op
  (Pil.ARRAY_ADDR op) ->
    (paren <$> tokenize (op ^. #base))
      <++> tokenize (op ^. #index)
      <++> tt "["
      <++> tt " slots of "
      <++> tokenize (op ^. #stride)
      <++> tt "bytes"
      <++> tt "]"
  (Pil.ASR op) -> tokenizeBinop msym "asr" op
  (Pil.BOOL_TO_INT op) -> tokenizeUnop msym "boolToInt" op
  (Pil.CEIL op) -> tokenizeUnop msym "ceil" op
  (Pil.CONST_BOOL op) -> pure [kt . show $ op ^. #constant]
  (Pil.CMP_E op) -> tokenizeBinopInfix msym "==" op
  (Pil.CMP_NE op) -> tokenizeBinopInfix msym "!=" op
  (Pil.CMP_SGE op) -> tokenizeBinopInfix msym ">=" op
  (Pil.CMP_SGT op) -> tokenizeBinopInfix msym ">" op
  (Pil.CMP_SLE op) -> tokenizeBinopInfix msym "<=" op
  (Pil.CMP_SLT op) -> tokenizeBinopInfix msym "<" op
  (Pil.CMP_UGE op) -> tokenizeBinopInfix msym "u>=" op
  (Pil.CMP_UGT op) -> tokenizeBinopInfix msym "u>" op
  (Pil.CMP_ULE op) -> tokenizeBinopInfix msym "u<=" op
  (Pil.CMP_ULT op) -> tokenizeBinopInfix msym "u<" op
  (Pil.CONST op) -> pure [setSym msym $ integerToken (op ^. #constant)]
  (Pil.CONST_PTR op) -> pure [setSym msym . addressToken Nothing $ fromIntegral (op ^. #constant)]
  (Pil.DIVS op) -> tokenizeBinopInfix msym "/" op
  (Pil.DIVS_DP op) -> tokenizeBinop msym "divsDP" op
  (Pil.DIVU op) -> tokenizeBinopInfix msym "u/" op
  (Pil.DIVU_DP op) -> tokenizeBinop msym "divuDP" op
  (Pil.FABS op) -> tokenizeUnop msym "fabs" op
  (Pil.FADD op) -> tokenizeBinopInfix msym "f+" op
  (Pil.FCMP_E op) -> tokenizeBinopInfix msym "f==" op
  (Pil.FCMP_GE op) -> tokenizeBinopInfix msym "f>=" op
  (Pil.FCMP_GT op) -> tokenizeBinopInfix msym "f>" op
  (Pil.FCMP_LE op) -> tokenizeBinopInfix msym "f<=" op
  (Pil.FCMP_LT op) -> tokenizeBinopInfix msym "f<" op
  (Pil.FCMP_NE op) -> tokenizeBinopInfix msym "f!=" op
  (Pil.FCMP_O op) -> tokenizeBinop msym "fcmpO" op
  (Pil.FCMP_UO op) -> tokenizeBinop msym "fcmpUO" op
  (Pil.FDIV op) -> tokenizeBinopInfix msym "f/" op
  (Pil.FIELD_ADDR op) ->
    parenExpr (op ^. #baseAddr)
      <++> [tt " + ", keywordToken "offset", tt " "]
      <++> tokenize (op ^. #offset)
  (Pil.CONST_FLOAT op) -> pure [plainToken FloatingPointToken $ show (op ^. #constant)]
  (Pil.FLOAT_CONV op) -> tokenizeUnop msym "floatConv" op
  (Pil.FLOAT_TO_INT op) -> tokenizeUnop msym "floatToInt" op
  (Pil.FLOOR op) -> tokenizeUnop msym "floor" op
  (Pil.FMUL op) -> tokenizeBinopInfix msym "f*" op
  (Pil.FNEG op) -> tokenizeUnop msym "fneg" op
  (Pil.FSQRT op) -> tokenizeUnop msym "fsqrt" op
  (Pil.FSUB op) -> tokenizeBinopInfix msym "f-" op
  (Pil.FTRUNC op) -> tokenizeUnop msym "ftrunc" op
  (Pil.IMPORT op) -> pure [addressToken Nothing $ fromIntegral (op ^. #constant)]
  (Pil.INT_TO_FLOAT op) -> tokenizeUnop msym "intToFloat" op
  (Pil.LOAD op) -> bracket <$> tokenize (op ^. #src)
  -- TODO: add memory versions for all SSA ops
  (Pil.LOW_PART op) -> tokenizeUnop msym "lowPart" op
  (Pil.LSL op) -> tokenizeBinop msym "lsl" op
  (Pil.LSR op) -> tokenizeBinop msym "lsr" op
  (Pil.MODS op) -> tokenizeBinopInfix msym "%" op
  (Pil.MODS_DP op) -> tokenizeBinop msym "modsDP" op
  (Pil.MODU op) -> tokenizeBinopInfix msym "u%" op
  (Pil.MODU_DP op) -> tokenizeBinop msym "moduDP" op
  (Pil.MUL op) -> tokenizeBinopInfix msym "*" op
  (Pil.MULS_DP op) -> tokenizeBinop msym "mulsDP" op
  (Pil.MULU_DP op) -> tokenizeBinop msym "muluDP" op
  (Pil.NEG op) -> tokenizeUnop msym "neg" op
  (Pil.NOT op) -> tokenizeUnop msym "not" op
  (Pil.OR op) -> tokenizeBinopInfix msym "|" op
  (Pil.POPCNT op) -> tokenizeUnop msym "popcnt" op
  -- TODO: Need to add carry
  (Pil.RLC op) -> tokenizeBinop msym "rlc" op
  (Pil.ROL op) -> tokenizeBinop msym "rol" op
  (Pil.ROR op) -> tokenizeBinop msym "ror" op
  (Pil.ROUND_TO_INT op) -> tokenizeUnop msym "roundToInt" op
  -- TODO: Need to add carry
  (Pil.RRC op) -> tokenizeBinop msym "rrc" op
  (Pil.SBB op) -> tokenizeBinop msym "sbb" op
  (Pil.STACK_LOCAL_ADDR op) ->
    pure
      [ tt "&"
      , varToken Nothing $ showStackLocalByteOffset (op ^. #stackOffset . #offset)
      ]
  (Pil.SUB op) -> tokenizeBinopInfix msym "-" op
  (Pil.SUB_WILL_OVERFLOW op) -> tokenizeBinop msym "subOF" op
  (Pil.SX op) -> tokenizeUnop msym "sx" op
  (Pil.TEST_BIT op) -> tokenizeBinop msym "testBit" op
  (Pil.UNIMPL t) -> keywordToken "unimpl" <++> paren [tt t]
  (Pil.UPDATE_VAR op) ->
    keywordToken "updateExpr" <++> (paren <$> parts)
    where
      arg name val more = [plainToken ArgumentNameToken name, tt ": "] <++> val <++> [tt ", " | more]
      parts = do
        arg "expr" (tokenize $ op ^. #src) True
          <++> arg "offset" (tokenize $ op ^. #offset) True
          <++> arg "val" (tokenize $ op ^. #src) False
  (Pil.VAR_JOIN op) ->
    tt "varJoin "
      <++> tokenize (op ^. #high)
      <++> tt " "
      <++> tokenize (op ^. #low)
  -- TODO: Need added
  (Pil.VAR op) -> tokenize (op ^. #src)
  -- TODO: Add field offset
  (Pil.VAR_FIELD op) -> tokenizeField op
  (Pil.XOR op) -> tokenizeBinop msym "xor" op
  (Pil.ZX op) -> tokenizeUnop msym "zx" op
  (Pil.CALL op) -> tokenize op
  (Pil.StrCmp op) -> tokenizeBinop msym "strcmp" op
  (Pil.StrNCmp op) ->
    [tt "strncmp ", integerToken (op ^. #len), tt " "]
      <++> tokenize (op ^. #left)
      <++> tt " "
      <++> tokenize (op ^. #right)
  (Pil.MemCmp op) -> tokenizeBinop msym "memcmp" op
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
    tokenizeExprOp (Just s) op (Pil.widthToSize bitwidth)
      <++> tt " :: "
      <++> fmap paren (tokenize s <++> tt " | " <++> mstype')
    where
      mstype' = maybe (pure [keywordToken "Unknown"]) tokenize mstype

instance Tokenizable (PI.InfoExpression (PI.SymInfo, Maybe PI.DeepSymType)) where
  tokenize (PI.InfoExpression (PI.SymInfo bitwidth s, mstype) op) =
    tokenizeExprOp (Just s) op (Pil.widthToSize bitwidth)
      <++> tt " :: "
      <++> (paren <$> tokenize s <++> tt " | " <++> mstype')
    where
      mstype' = maybe (pure [keywordToken "Unknown"]) tokenize mstype

instance Tokenizable (PI.InfoExpression PI.SymInfo) where
  tokenize (PI.InfoExpression (PI.SymInfo bitwidth (PI.Sym n)) op) =
    [tt (show n), tt ":"]
      <++> (paren <$> tokenizeExprOp (Just $ PI.Sym n) op (Pil.widthToSize bitwidth))

instance Tokenizable Pil.Expression where
  tokenize (Pil.Expression size' exprOp) = tokenizeExprOp Nothing exprOp size'

instance (Tokenizable a, Tokenizable b) => Tokenizable (HashMap a b) where
  tokenize m =
    [keywordToken "HashMap:", tt "\n"]
      <++> (intercalate [tt "\n"] <$> traverse f (HashMap.toList m))
    where
      f pair = tt "  " <++> tokenize pair

instance
  ( Tokenizable a
  , HasField' "size" a (Pil.Size a)
  , NeedsParens a
  ) =>
  Tokenizable (Pil.Statement a)
  where
  tokenize stmt = case stmt of
    Pil.Def x ->
      tokenize (x ^. #var)
        <++> tt " = "
        <++> tokenize (x ^. #value)
    Pil.Constraint x -> tt "?: " <++> tokenize (x ^. #condition)
    Pil.Store x ->
      (bracket <$> tokenize (x ^. #addr))
        <++> tt "."
        <++> decimalToken (Pil.sizeToWidth $ x ^. #value . #size)
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
    Pil.EnterContext x -> tt "----> Entering "
      <++> tokenize (x ^. #ctx)
      <++> [tt " "]
      <++> tokenizeAsList (x ^. #args)
    Pil.ExitContext x -> tt "<---- Leaving " <++> tokenize (x ^. #leavingCtx)
    Pil.Call callOp -> tokenize callOp
    Pil.DefPhi x ->
      tokenize (x ^. #dest)
        <++> [tt " = ", keywordToken "φ"]
        <++> tokenizeAsCurlyList (x ^. #src)
    Pil.DefMemPhi x ->
      varToken Nothing ("mem#" <> show (x ^. #destMemory))
        <++> [tt " = ", tt "φ"]
        <++> tokenizeAsCurlyList ((\m -> varToken Nothing $ "mem#" <> show m) <$> (x ^. #srcMemory))
    Pil.BranchCond x -> [keywordToken "if", tt " "] <++> (paren <$> tokenize (x ^. #cond))
    Pil.Jump x -> [keywordToken "jump", tt " "] <++> parenExpr (x ^. #dest)
    Pil.JumpTo x ->
      parenExpr (x ^. #dest)
        <++> [keywordToken " jumpTo", tt " "]
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
          <++> tokenizeAsTuple (op ^. #args)
      (Just name, Pil.CallExpr ce) ->
        tokenize name <++> tt "{" <++> tokenize ce <++> tt "}"
          <++> tokenizeAsTuple (op ^. #args)
      _ ->
        [keywordToken "call", tt " "]
          <++> tokenize (op ^. #dest)
          <++> tokenizeAsTuple (op ^. #args)

instance Tokenizable Pil.CallSite where
  tokenize x =
    tokenize (x ^. #caller)
      <++> tt " -> "
      <++> tokenize (x ^. #callDest)

newtype PStmts a = PStmts [Pil.Statement a]

newtype PIndexedStmts a = PIndexedStmts [(Int, Pil.Statement a)]

tokenizeStmtsWithIndents
  :: forall a.
  ( Tokenizable a
  , HasField' "size" a (Pil.Size a)
  , NeedsParens a
  ) 
  => [Pil.Statement a]
  -> Tokenizer [Token]
tokenizeStmtsWithIndents = \case
  [] -> return []
  (stmt:stmts) -> tokenize stmt
    <++> [tt "\n"]
    <++> go (bool 0 1 $ isEnterContext stmt) stmts
  where
    isEnterContext (Pil.EnterContext _) = True
    isEnterContext _ = False
    spaces :: Int -> [Token]
    spaces n = [tt $ Text.replicate (max 0 n) "  "]
    go :: Int -> [Pil.Statement a] -> Tokenizer [Token]
    go _ [] = return []
    -- Last Stmt
    go i [stmt] = case stmt of
      Pil.ExitContext _ -> spaces (i - 1) <++> tokenize stmt
      _ -> tokenize stmt
    -- Middle statements
    go i (stmt:stmts) = case stmt of
      Pil.EnterContext _ ->
        spaces i <++> tokenize stmt <++> [tt "\n"] <++> go (i + 1) stmts
      Pil.ExitContext _ ->
        spaces (i - 1) <++> tokenize stmt <++> [tt "\n"] <++> go (i - 1) stmts
      _ -> spaces i <++> tokenize stmt <++> [tt "\n"] <++> go i stmts

instance
  ( Tokenizable a
  , HasField' "size" a (Pil.Size a)
  , NeedsParens a
  ) =>
  Tokenizable (PStmts a)
  where
  tokenize (PStmts stmts) = tokenizeStmtsWithIndents stmts
  -- tokenize (PStmts stmts) = intercalate [tt "\n"] <$> traverse tokenize stmts

instance
  ( Tokenizable a
  , HasField' "size" a (Pil.Size a)
  , NeedsParens a
  ) =>
  Tokenizable (PIndexedStmts a)
  where
  tokenize (PIndexedStmts stmts) = intercalate [tt "\n"] <$> traverse f (sortOn fst stmts)
   where
    f :: (Int, Pil.Statement a) -> Tokenizer [Token]
    f (i, stmt) = [tt (show i), tt ": "] <++> tokenize stmt

instance Tokenizable ByteOffset where
  tokenize (ByteOffset n) = pure [integerToken n]

instance Tokenizable Bits where
  tokenize (Bits n) = pure [integerToken n]

instance Tokenizable Bytes where
  tokenize (Bytes n) = pure [integerToken n]

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

instance Tokenizable Word64 where
  tokenize x = pure [integerToken x]

instance Tokenizable t => Tokenizable (PI.PilType t) where
  tokenize = \case
    PI.TArray len elemType ->
      [kt "Array", tt " "]
        <++> tokenize len
        <++> tt " "
        <++> tokenize elemType
    --    PI.TZeroField pt -> "ZeroField" <-> paren (tokenize pt)
    PI.TBool -> pure [kt "Bool"]
    PI.TChar bitWidth -> [kt "Char"] <++> tokenize bitWidth
    -- PI.TQueryChar -> "QueryChar"
    PI.TInt bitWidth signed -> return [kt $ intName <> intWidth]
      where
        intName = case signed of
          Nothing -> "_Int"
          Just True -> "SInt"
          Just False -> "UInt"
        intWidth = showAsInt_ bitWidth
    PI.TFloat bitWidth -> return [kt "Float", tt $ showAsInt_ bitWidth]
    PI.TBitVector bitWidth -> return [kt "BitVector", tt $ showAsInt_ bitWidth]
    PI.TPointer bitWidth pointeeType ->
      [kt "Pointer", tt $ showAsInt_ bitWidth]
        <++> tt " "
        <++> (paren <$> tokenize pointeeType)
    PI.TCString len -> [kt "CString", tt " "] <++> tokenize len
    PI.TRecord m ->
      [kt "Record", tt " "]
        <++> ( delimitedList [tt "["] [tt ", "] [tt "]"]
                <$> traverse rfield (sortOn fst $ HashMap.toList m)
             )
      where
        rfield :: forall a. Tokenizable a => (BitOffset, a) -> Tokenizer [Token]
        rfield (BitOffset n, t) = paren <$> [tt (show n), tt ", "] <++> tokenize t
    PI.TBottom s -> paren <$> [kt "Bottom", tt " "] <++> tokenize s
    PI.TUnit -> pure [kt "Unit"]
    PI.TFunction _ret _params -> pure [kt "Func"]

-- | Shows something as an Integer or, if Nothing, as an underscore
showAsInt_ :: (Integral a, IsString b, StringConv String b) => Maybe a -> b
showAsInt_ = maybe "_" showAsInt

showAsInt :: (Integral a, StringConv String b) => a -> b
showAsInt n = show (fromIntegral n :: Integer)

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
          , typeSym = Nothing
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
    Grouping n -> tokenize n

instance Tokenizable a => Tokenizable (CfEdge a) where
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

instance Tokenizable (GroupingNode a) where
  tokenize (GroupingNode _termNode _uuid' _grouping _nodeData) =
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

-- | This matches each node to an Int and uses the Int to show the edges
instance (Hashable a, Tokenizable a) => Tokenizable (Cfg (CfNode a)) where
  tokenize cfg =
    [tt "---CFG---\n", tt "--- Node Mapping:\n"]
      <++> showNodeMapping
      <++> tt "\n--- Edges:\n"
      <++> showEdges
      <++> tt "\n--- Attrs:\n"
      <++> showAttrs
      <++> tt ("\n--- Next Ctx Index: " <> show (fromIntegral $ cfg ^. #nextCtxIndex :: Int))
    where
      cflow = cfg ^. #graph

      nodeMapList :: [(CfNode a, Int)]
      nodeMapList = zip (HashSet.toList $ G.nodes cflow) [0..]

      nodeMap :: HashMap (CfNode a) Int
      nodeMap = HashMap.fromList nodeMapList

      showNodeMapping :: Tokenizer [Token]
      showNodeMapping = intercalate [tt "\n"] <$> traverse showNode nodeMapList

      showNode :: (CfNode a, Int) -> Tokenizer [Token]
      showNode (node, nid) = [tt (show nid), tt " : "] <++> tokenize node

      showEdges :: Tokenizer [Token]
      showEdges =
        pure
          [ tt
              . Text.concat
              . fmap (cs . pshow . fmap (fromJust . flip HashMap.lookup nodeMap))
              . G.edges
              $ cflow
          ]

      showAttrs :: Tokenizer [Token]
      showAttrs = intercalate [tt "\n"] <$> sequence (mapMaybe showAttr nodeMapList)

      showAttr :: forall t. (Show t) => (CfNode a, t) -> Maybe (Tokenizer [Token])
      showAttr (node, nid) = do
        return $ [tt (show nid), tt " : "] <++> tokenizeAsList (toList node)

newtype FullCfNode a = FullCfNode (CfNode a)
  deriving newtype (Eq, Ord, Show, Generic, Hashable)

instance G.Identifiable (FullCfNode a) UUID where
  getNodeId (FullCfNode x) = G.getNodeId x

-- | Displays node and attrs
instance Tokenizable a => Tokenizable (FullCfNode [a]) where
  tokenize (FullCfNode cfn) = tokenize cfn <++> tt "\n" <++> tokenizeAsNewlinedList (Cfg.getNodeData cfn)

instance
  ( Hashable a
  , Tokenizable a
  , Show a
  , Ord i
  , Hashable i
  , Tokenizable l
  , G.Identifiable a i) => Tokenizable (AlgaPath l i a) where
  tokenize p = tt "--- Path ---\n"
               <++> tokenize rootNode
               <++> showEdges edges
    where
      (rootNode, edges) = Path.toEdgeList p
      showEdges :: [G.LEdge l a] -> Tokenizer [Token]
      showEdges [] = return []
      showEdges ((G.LEdge lbl (G.Edge _ b)):es) =
        [ tt "\n>----| " ]
        <++> tokenize lbl
        <++> tt " |---->\n"
        <++> tokenize b
        <++> showEdges es

instance
  ( Hashable a
  , Show a
  , Tokenizable a
  , G.Identifiable a UUID) => Tokenizable (CfgPath.Path a) where
  tokenize p = tt "--- CtxPath ---"
               <++> tt "\nouterCtx: " <++> tokenize (p ^. #outerCtx)
               <++> tt "\nnextCtxIndex: " <++> tokenize (fromIntegral $ p ^. #nextCtxIndex :: Int)
               <++> tt "\n" <++> tokenize (p ^. #path)


-- | Tokenizable print to IO.
prettyPrint :: (MonadIO m, Tokenizable a) => TokenizerCtx -> a -> m ()
prettyPrint ctx = putText . pretty ctx

prettyPrint' :: (MonadIO m, Tokenizable a) => a -> m ()
prettyPrint' = prettyPrint blankTokenizerCtx

pp :: (MonadIO m, Tokenizable a) => TokenizerCtx -> a -> m ()
pp = prettyPrint

pp' :: (MonadIO m, Tokenizable a) => a -> m ()
pp' = prettyPrint'

prettyStmts ::
  ( MonadIO m
  , Tokenizable a
  , HasField' "size" a (Pil.Size a)
  , NeedsParens a
  ) =>
  TokenizerCtx ->
  [Pil.Statement a] ->
  m ()
prettyStmts ctx = prettyPrint ctx . PStmts

prettyStmts' ::
  ( MonadIO m
  , Tokenizable a
  , HasField' "size" a (Pil.Size a)
  , NeedsParens a
  ) =>
  [Pil.Statement a] ->
  m ()
prettyStmts' = prettyPrint' . PStmts

prettyIndexedStmts ::
  ( MonadIO m
  , Tokenizable a
  , HasField' "size" a (Pil.Size a)
  , NeedsParens a
  ) =>
  TokenizerCtx ->
  [(Int, Pil.Statement a)] ->
  m ()
prettyIndexedStmts ctx = prettyPrint ctx . PIndexedStmts

prettyIndexedStmts' ::
  ( MonadIO m
  , Tokenizable a
  , HasField' "size" a (Pil.Size a)
  , NeedsParens a
  ) =>
  [(Int, Pil.Statement a)] ->
  m ()
prettyIndexedStmts' = prettyPrint' . PIndexedStmts

instance Tokenizable CV where
  tokenize cv = pure [tt $ show cv]

instance Tokenizable SVal where
  tokenize x = pure [tt $ show x]

instance Tokenizable a => Tokenizable (Maybe a) where
  tokenize Nothing = pure [tt "Nothing"]
  tokenize (Just x) = [tt "Just", tt " "] <++> tokenize x

instance Tokenizable a => Tokenizable (G.Dominators a) where
  tokenize (G.Dominators m) = tokenize m

instance Tokenizable a => Tokenizable (G.PostDominators a) where
  tokenize (G.PostDominators m) = tokenize m

instance Tokenizable Bool where
  tokenize b = pure [tt $ show b]

instance Tokenizable Text where
  tokenize t = pure [tt t]

instance Tokenizable (GCfg.GroupSpec a) where
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

instance Tokenizable BitOffset where
  tokenize (BitOffset n) = tokenize n

instance Tokenizable a => Tokenizable (PI.UnifyError a) where
  tokenize = \case
    (PI.UnifyError t1 t2 err) ->
      [tt "UnifyError", tt " "]
      <++> (paren <$> tokenize t1)
      <++> tt " "
      <++> (paren <$> tokenize t2)
      <++> tt " "
      <++> tokenize err
    (PI.IncompatibleTypes t1 t2) ->
      [tt "IncompatibleTypes", tt " "]
      <++> (paren <$> tokenize t1)
      <++> tt " "
      <++> (paren <$> tokenize t2)
    PI.IncompatibleTypeLevelValues -> return [tt "UnifyError"]
    (PI.OverlappingRecordField recFields badOffsets) ->
      [tt "OverlappingRecordField", tt "\n"]
      <++> tokenize recFields
      <++> tt " "
      <++> tokenize badOffsets

instance Tokenizable a => Tokenizable (PI.UnifyConstraintsError a) where
  tokenize x = [tt "UnifyConstraintsError", tt " "]
    <++> [tt "stmtOrigin", tt " "] <++> tokenize (x ^. #stmtOrigin)
    <++> [tt "sym", tt " "] <++> tokenize (x ^. #sym)
    <++> [tt "error", tt " "] <++> tokenize (x ^. #error)

instance Tokenizable UUID where
  tokenize x = return [tt $ show x]

instance Tokenizable LoadExpr where
  tokenize (LoadExpr x) = tokenize x

instance Tokenizable Effect where
  tokenize eff =
    let (name, stmt) = case eff of
          (EffectWrite s) -> ("Write", s)
          (EffectAlloc s) -> ("Alloc", s)
          (EffectDealloc s) -> ("Dealloc", s)
          (EffectCall s) -> ("Call", s)
    in
        [tt "<", tt name, tt ">"] <++> tokenize stmt
