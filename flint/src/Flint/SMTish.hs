{-# OPTIONS_GHC -Wno-partial-fields #-}

module Flint.SMTish where

import Flint.Prelude hiding (bracket)

import qualified Flint.Types.Analysis.Path.Matcher.Primitives as Prim
import Flint.Types.Analysis.Path.Matcher.Primitives (CallableWMI, PrimSpec, FuncVarExpr, FuncVarExprSize, FuncVar)
import Blaze.Pretty hiding (tokenizeBinop, tokenizeBinopInfix, tokenizeUnop, tokenizeField, tokenizeExprOp)

import Blaze.Pil.Construct (ExprConstructor(..))
import Blaze.Pil.Display (needsParens, NeedsParens)
import qualified Blaze.Types.Function as Function
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil.PilType (Sym)

import Data.Aeson (ToJSON(toJSON), object)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Numeric

import qualified Data.String as String


instance Tokenizable SMTishExpression where
  tokenize expr = tokenizeExprOp Nothing (expr ^. #op) (expr ^. #size)

instance NeedsParens SMTishExpression where
  needsParens = needsParens . view #op

data SMTishExpression = SMTishExpression
  { size :: Pil.Size Pil.Expression
  , op :: Pil.ExprOp SMTishExpression
  } deriving (Eq, Ord, Show, Hashable, Generic)

class ToSMTishExpression a where
  toSMTishExpression :: a -> SMTishExpression

instance ToSMTishExpression Pil.Expression where
  toSMTishExpression (Pil.Expression sz op) = SMTishExpression sz (toSMTishExpression <$> op)

instance ExprConstructor (Pil.Size Pil.Expression) SMTishExpression where
  mkExpr = SMTishExpression

-- | Prefix lisp-like binop
tokenizeBinop ::
  ( Tokenizable b
  , HasField' "left" a b
  , HasField' "right" a b
  ) =>
  Maybe Sym ->
  Text ->
  a ->
  Tokenizer [Token]
tokenizeBinop tsym opSym op = fmap paren $ 
  setSym tsym (instr opSym)
    <++> tt " "
    <++> tokenize (op ^. #left)
    <++> tt " "
    <++> tokenize (op ^. #right)

-- | This version of showHex will pad out the digits to match the size
showHexSMT :: Integral a => Bytes -> a -> Text
showHexSMT sz x = "0x" <> pad <> h
  where
    pad = Text.replicate padLen "0"
    padLen = 2 * fromIntegral sz - Text.length h
    h = cs $ Numeric.showHex (fromIntegral x :: Word64) ""

-- | Prefix lisp-like binop, but with the underscore first like some SMT funcs have
tokenizeUnderBinop ::
  ( Tokenizable b
  , HasField' "left" a b
  , HasField' "right" a b
  ) =>
  Maybe Sym ->
  Text ->
  a ->
  Tokenizer [Token]
tokenizeUnderBinop tsym opSym op = fmap paren $ 
         tt "(_ "
    <++> setSym tsym (instr opSym)
    <++> tt " "
    <++> tokenize (op ^. #left)
    <++> tt ")"
    <++> tt " "
    <++> tokenize (op ^. #right)

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
tokenizeUnop tsym opSym op = fmap paren $ setSym tsym (instr opSym)
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

tokenizeExprOp
  :: (NeedsParens a, Tokenizable a)
  => Maybe Sym
  -> Pil.ExprOp a
  -> Pil.Size Pil.Expression
  -> Tokenizer [Token]
tokenizeExprOp msym exprOp size = case exprOp of
  -- | todo
  (Pil.ADC op) -> tokenizeBinop msym "adc" op

  (Pil.ADD op) -> tokenizeBinop msym "bvadd" op

  -- | todos
  (Pil.ADD_WILL_CARRY op) -> tokenizeBinop msym "addOF" op
  (Pil.ADD_WILL_OVERFLOW op) -> tokenizeBinop msym "addCF" op


  (Pil.AND op) -> tokenizeBinop msym "bvand" op

  -- | (ARRAY_ADDR <base_addr expr> <index expr> <stride>)
  (Pil.ARRAY_ADDR op) -> fmap paren $
    tt "ARRAY_ADDR"
    <++> tt " "
    <++> parenExpr (op ^. #base)
    <++> tt " "
    <++> parenExpr (op ^. #index)
    <++> tt " "
    <++> tt (show $ op ^. #stride)

  (Pil.ASR op) -> tokenizeBinop msym "bvashr" op

  -- | todos
  (Pil.BOOL_TO_INT op) -> tokenizeUnop msym "boolToInt" op
  (Pil.CEIL op) -> tokenizeUnop msym "ceil" op
  (Pil.CONST_BOOL op) -> pure [kt . show $ op ^. #constant]

  
  (Pil.CMP_E op) -> tokenizeBinop msym "=" op
  (Pil.CMP_NE op) -> tt "(not " <++> tokenizeBinop msym "=" op <++> tt ")"
  (Pil.CMP_SGE op) -> tokenizeBinop msym "bvsge" op
  (Pil.CMP_SGT op) -> tokenizeBinop msym "bvsgt" op
  (Pil.CMP_SLE op) -> tokenizeBinop msym "bvsle" op
  (Pil.CMP_SLT op) -> tokenizeBinop msym "bvslt" op
  (Pil.CMP_UGE op) -> tokenizeBinop msym "bvuge" op
  (Pil.CMP_UGT op) -> tokenizeBinop msym "bvugt" op
  (Pil.CMP_ULE op) -> tokenizeBinop msym "bvule" op
  (Pil.CMP_ULT op) -> tokenizeBinop msym "bvult" op
  (Pil.CONST op) -> pure [tt $ showHexSMT (toBytes_ size) $ op ^. #constant]
  (Pil.CONST_PTR op) -> pure [setSym msym . addressToken Nothing $ intToAddr (op ^. #constant)]
  (Pil.DIVS op) -> tokenizeBinop msym "bvsdiv" op
  (Pil.DIVS_DP op) -> tokenizeBinop msym "/" op
  (Pil.DIVU op) -> tokenizeBinop msym "/" op
  (Pil.DIVU_DP op) -> tokenizeBinop msym "/" op

  -- | todo
  (Pil.FABS op) -> tokenizeUnop msym "fabs" op

  (Pil.FADD op) -> tokenizeBinop msym "+" op
  (Pil.FCMP_E op) -> tokenizeBinop msym "=" op
  (Pil.FCMP_GE op) -> tokenizeBinop msym ">=" op
  (Pil.FCMP_GT op) -> tokenizeBinop msym ">" op
  (Pil.FCMP_LE op) -> tokenizeBinop msym "<=" op
  (Pil.FCMP_LT op) -> tokenizeBinop msym "<" op

  (Pil.FCMP_NE op) -> tt "(not " <++> tokenizeBinop msym "=" op <++> tt ")"

  -- we don't even use these when converting from Ghidra
  (Pil.FCMP_O op) -> tokenizeBinop msym "fp.isZero" op
  (Pil.FCMP_UO op) -> tokenizeBinop msym "fp.isZero" op

  (Pil.FDIV op) -> tokenizeBinop msym "/" op
  (Pil.FIELD_ADDR op) -> fmap paren $
    tt "FIELD_ADDR"
    <++> tt " "
    <++> parenExpr (op ^. #baseAddr)
    <++> tt " "
    <++> tt (showHexSMT (toBytes_ size) $ op ^. #offset)

  (Pil.CONST_FLOAT op) -> pure [plainToken FloatingPointToken $ show (op ^. #constant)]
  (Pil.FLOAT_CONV op) -> tokenizeUnop msym "floatConv" op
  (Pil.FLOAT_TO_INT op) -> tokenizeUnop msym "floatToInt" op
  (Pil.FLOOR op) -> tokenizeUnop msym "floor" op
  (Pil.FMUL op) -> tokenizeBinop msym "*" op
  (Pil.FNEG op) -> tokenizeUnop msym "fneg" op
  (Pil.FSQRT op) -> tokenizeUnop msym "fsqrt" op
  (Pil.FSUB op) -> tokenizeBinop msym "-" op
  (Pil.FTRUNC op) -> tokenizeUnop msym "ftrunc" op

  (Pil.GLOBAL_PTR op) -> return $ 
    [ tt "GLOBAL {addr: "
    , tt (showHexSMT (toBytes_ size) $ op ^. #constant)
    ] <> case op ^. #symbol of
    Nothing -> [tt "}"]
    Just t ->
      [ tt ", name: \""
      , tt t
      , tt "\"}"
      ]
    
  (Pil.IMPORT op) -> pure [addressToken Nothing $ intToAddr (op ^. #constant)]
  (Pil.INT_TO_FLOAT op) -> tokenizeUnop msym "intToFloat" op
  (Pil.LOAD op) -> fmap paren $
    tt "DEREF"
    <++> tt " "
    <++> decimalToken (Pil.sizeToWidth size)
    <++> tt " "
    <++> tokenize (op ^. #src)

    

  -- TODO: add memory versions for all SSA ops
  (Pil.LOW_PART op) -> tokenizeUnop msym ("(_ extract " <> showHex (size - 1) <> " 0)") op
  (Pil.LSL op) -> tokenizeBinop msym "bvshl" op
  (Pil.LSR op) -> tokenizeBinop msym "bvlshr" op
  (Pil.MODS op) -> tokenizeBinop msym "bvsmod" op
  (Pil.MODS_DP op) -> tokenizeBinop msym "bvsmod" op
  (Pil.MODU op) -> tokenizeBinop msym "mod" op
  (Pil.MODU_DP op) -> tokenizeBinop msym "mod" op
  (Pil.MUL op) -> tokenizeBinop msym "*" op
  (Pil.MULS_DP op) -> tokenizeBinop msym "*" op
  (Pil.MULU_DP op) -> tokenizeBinop msym "*" op
  (Pil.NEG op) -> tokenizeUnop msym "bvneg" op
  (Pil.NOT op) -> tokenizeUnop msym "bvnot" op
  (Pil.OR op) -> tokenizeBinop msym "bvor" op
  (Pil.POPCNT op) -> tokenizeUnop msym "popcnt" op

  -- sadly, the bitvector theories doesn't have these with-carry versions
  (Pil.RLC op) -> tokenizeUnderBinop msym "rotate_left_carry" op
  (Pil.ROL op) -> tokenizeUnderBinop msym "rotate_left" op
  (Pil.ROR op) -> tokenizeUnderBinop msym "rotate_right" op

  -- todo
  (Pil.ROUND_TO_INT op) -> tokenizeUnop msym "roundToInt" op

  (Pil.RRC op) -> tokenizeUnderBinop msym "rotate_right_carry" op

  -------- bookmark ---------------
  (Pil.SBB op) -> tokenizeBinop msym "sbb" op
  (Pil.STACK_LOCAL_ADDR op) ->
    pure
      [ tt "&"
      , varToken Nothing $ showStackLocalByteOffset (op ^. #stackOffset . #offset)
      ]
  (Pil.SUB op) -> tokenizeBinop msym "-" op
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

data Effect = Effect
  { effectType :: Text
  , effectId :: Text
  , vars :: EffectVars
  } deriving (Eq, Ord, Read, Show, Generic, Hashable)

instance ToJSON Effect where
  toJSON (Effect etype eid vars) =
        object [ "type" A..= etype
               , "id" A..= eid
               , "arguments" A..= toJSON vars
               ]

-- From the greater system spec
data EffectVars
  = Write { destinations :: [Text]
          , offset_start :: Text
          , offset_end :: Text
          , max_length :: Text
          , values :: [Text]
          }
  | Read { locations :: [Text]
         , offset_start :: Text
         , offset_end :: Text
         , max_length :: Text
         }
  | Malloc { locations :: [Text]
           , size :: Text
           }
  | Free { locations :: [Text] }
  | CopyPtr { sources :: [Text]
            , destinations :: [Text]
            , offset :: Text
            }
  | CopyMem { sources :: [Text]
            , destinations :: [Text]
            , length :: Text
            }
  | AddOffsetPtr
    { locations :: [Text]
    , offset :: Text
    }
  | Other (HashMap Text Text) -- var/funcvar mapping
  deriving (Eq, Ord, Read, Show, Hashable, Generic)

instance ToJSON EffectVars where
  toJSON (Other m) = object $ (\(k, v) -> String.fromString (cs k :: String) A..= v) <$> HashMap.toList m
  toJSON x = over #_Object (KM.delete "tag") $ A.genericToJSON A.defaultOptions x

data Operation = Operation
  { variables :: [Text]
  , effects :: [Effect]
  , preconditions :: [Text]
  } deriving (Eq, Ord, Read, Show, Generic, Hashable, ToJSON)

data SMTishWMI = SMTishWMI
  { operation :: Operation
  , name :: Text
  , trigger :: Text
  } deriving (Eq, Ord, Read, Show, Generic, Hashable, ToJSON)

data SMTishFuncVarExpr
  = FuncVar FuncVar
  | FuncVarExpr FuncVarExprSize (Pil.ExprOp SMTishFuncVarExpr)
  deriving (Eq, Ord, Show, Hashable, Generic)

instance NeedsParens SMTishFuncVarExpr where
  needsParens (FuncVar Prim.Ret) = False
  needsParens (FuncVar (Prim.Arg _)) = True
  needsParens (FuncVar (Prim.Global _)) = True
  needsParens (FuncVarExpr _ op) = needsParens op

instance Tokenizable SMTishFuncVarExpr where
  tokenize (FuncVar (Prim.Arg n)) = return [tt $ "(ARG " <> show n <> ")"]
  tokenize (FuncVar (Prim.Global x)) = return [tt $ "(" <> pretty' (toSMTishExpression x) <> ")"]
  tokenize (FuncVar Prim.Ret) = return [tt "RET"]
  tokenize (FuncVarExpr sz op) = tokenizeExprOp Nothing op (lookupSize sz)
    where
      lookupSize (Prim.ConstSize n) = n
      -- Is this correct? should the size of the global be the size of the expr
      -- that represents it's address? i think so..
      lookupSize (Prim.SizeOf (Prim.Global x)) = x ^. #size
      -- TODO: We don't have information re size of RET and ARGS without a callsite.
      -- It would be nice if this was part of our Blaze Function type.
      -- This could be fixed by issue 344
      lookupSize (Prim.SizeOf Prim.Ret) = 8
      lookupSize (Prim.SizeOf (Prim.Arg _)) = 8

      


toSMTishFuncVarExpr :: FuncVarExpr -> SMTishFuncVarExpr
toSMTishFuncVarExpr (Prim.FuncVar v) = FuncVar v
toSMTishFuncVarExpr (Prim.FuncVarExpr sz op) = FuncVarExpr sz $ toSMTishFuncVarExpr <$> op


-- | Returns (effect name, EffectVars), according to the greater system spec
toEffectVars :: Text -> HashMap Text Text -> (Text, EffectVars)
toEffectVars primName vars = case primName of
  "copy" -> ("copyMem", CopyMem (vs "src") (vs "dest") (v "len"))
  "freeHeap" -> ("free", Free (vs "ptr"))
  "allocHeap" -> ("malloc", Malloc (vs "ptr") (v "size"))
  -- "Write" -> Write (vs "ptr") "0x0" "0x0" 
  _ -> (primName, Other vars)
  where
    v x = case HashMap.lookup x vars of
      Nothing -> error $ "Couldn't find var '" <> cs x <> "' in hashmap for '" <> cs primName <> "' primitive"
      Just y -> y
    vs x = [v x]

callableWMIToEffect :: CallableWMI -> Effect
callableWMIToEffect x = Effect
  { effectType = name
  , effectId = show $ hash x
  , vars = vars'
  }
  where
    (name, vars') = toEffectVars primName
      . HashMap.fromList
      . fmap (bimap pretty' (pretty' . toSMTishFuncVarExpr . fst))
      . HashMap.toList
      $ x ^. #varMapping
    primName = x ^. #prim . #name

-- | This just creates a trigger that is essentially a call to the function.
-- It's up to the WMI composer to look at the preconditions from that point
getTriggerAsFuncCall :: CallableWMI -> Text
getTriggerAsFuncCall x = funcName <> "(" <> Text.intercalate ", " arglist <> ");"
  where
    arglist = fmap (\n -> "(ARG " <> show n <> ")") [0..(funcParamCount - 1)]
    funcName = x ^. #func . Function._name
    funcParamCount = Flint.Prelude.length $ x ^. #func . Function._params

toSMTishWMI :: CallableWMI -> SMTishWMI
toSMTishWMI x = SMTishWMI
  { trigger = getTriggerAsFuncCall x
  , name = x ^. #prim . #name
  , operation = Operation
    { variables = ["; variables section\n; TODO\n"]
    -- | TODO: at some point, a CallableWMI might have multiple effects, too.
    -- But for now, each CallableWMI is just one effect.
    , effects = [callableWMIToEffect x]
    , preconditions = pretty' . toSMTishFuncVarExpr . fst <$> x ^. #constraints
    }
  }

data FlintSMTishResult = FlintSMTishResult
  { baseAddress :: Address
  , operations :: [SMTishWMI] -- [("prim name", [prims])]
  } deriving (Eq, Ord, Show, Generic, ToJSON)

toFlintSMTishResult
  :: Address
  -> HashMap PrimSpec (HashSet CallableWMI)
  -> FlintSMTishResult
toFlintSMTishResult baseOffset
  = FlintSMTishResult baseOffset
    . fmap toSMTishWMI
    . HashSet.toList
    . foldl' HashSet.union HashSet.empty
    . HashMap.elems
