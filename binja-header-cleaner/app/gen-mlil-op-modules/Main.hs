module Main where

import Binja.Header.Prelude hiding (FilePath, handle, onException, (<.>))
import qualified Prelude as P

-- import qualified System.Filepath as Path

import System.Directory (createDirectoryIfMissing)
import System.FilePath (FilePath, (<.>), (</>))

import Binja.Header.Types.Printer (Printer, br, indent, pr)
import qualified Binja.Header.Types.Printer as Printer
import qualified Data.Char as Char
import Data.List (nub)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Text.Casing as Casing

main :: IO ()
main = do
  args <- getArgs
  case args of
    [outDir] -> writeRecords outDir
    _ -> putText "gen-mlil-op-modules <outDir>"

opTypeType :: Text -> Text
opTypeType ot = case ot of
  "int" -> "Int64"
  "float" -> "Double"
  "expr" -> "expr"
  "intrinsic" -> "Intrinsic"
  "var" -> "Variable"
  "var_ssa" -> "SSAVariable"
  "var_ssa_dest_and_src" -> "SSAVariableDestAndSrc"
  "int_list" -> "[Int64]"
  "address_list" -> "[Address]"
  "var_list" -> "[Variable]"
  "var_ssa_list" -> "[SSAVariable]"
  "expr_list" -> "[expr]"
  _ -> P.error "opTypeType: no case match"

opTypeBuilder :: Text -> Text
opTypeBuilder ot = case ot of
  "int" -> "buildInt"
  "float" -> "buildFloat"
  "expr" -> "buildExpr"
  "intrinsic" -> "buildIntrinsinc"
  "var" -> "buildVariable"
  "var_ssa" -> "buildSSAVariable"
  "var_ssa_dest_and_src" -> "buildSSAVariableDestAndSrc"
  "int_list" -> "buildIntList"
  "address_list" -> "buildAddressList"
  "var_list" -> "buildVarList"
  "var_ssa_list" -> "buildSSAVarList"
  "expr_list" -> "buildExprList"
  _ -> P.error "opTypeBuilder: no case match"

capFirst :: String -> String
capFirst "" = ""
capFirst (x : xs) = Char.toUpper x : xs

capFirstText :: Text -> Text
capFirstText = Text.pack . capFirst . Text.unpack

operatorNameToConstructorName :: Text -> Text
operatorNameToConstructorName =
  Text.pack . Casing.toScreamingSnake . Casing.dropPrefix . Casing.fromSnake . Text.unpack

operatorNameToPrefixName :: Text -> Text
operatorNameToPrefixName =
  Text.replace "Ssa" "SSA"
    . (<> "Op")
    . Text.pack
    . Casing.toCamel
    . Casing.dropPrefix
    . Casing.fromSnake
    . Text.unpack

operatorNameToRecordName :: Text -> Text
operatorNameToRecordName =
  Text.replace "Ssa" "SSA"
    . (<> "Op")
    . Text.pack
    . capFirst
    . Casing.toCamel
    . Casing.dropPrefix
    . Casing.fromSnake
    . Text.unpack

operatorRecordList :: [Text]
operatorRecordList = operatorNameToRecordName . fst <$> statementsData

printRestArgs :: Text -> [(Text, Text)] -> Printer ()
printRestArgs _ [] = pr $ "} " <> derivingClause
printRestArgs pname ((argName, argType) : args) = do
  pr $ ", _" <> pname <> capFirstText argName <> " :: " <> opTypeType argType
  printRestArgs pname args

derivingClause :: Text
derivingClause = "deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)"

printOperationUnionType :: [(Text, [(Text, Text)])] -> Printer ()
printOperationUnionType (firstOp : moreOps) = do
  pr "data Operation expr"
  indent $ do
    pr $ "= " <> strOp firstOp
    printRest moreOps
  where
    printRest [] = pr "deriving (Eq, Ord, Show, Functor, Foldable, Traversable)"
    printRest (op : ops) = do
      pr $ "| " <> strOp op
      printRest ops
    strOp (opName, args) =
      operatorNameToConstructorName opName
        <> bool (" (" <> operatorNameToRecordName opName <> " expr)") "" (null args)
printOperationUnionType _ = P.error "printOperationUnionType: expecting non-empty list"

printOpRecordDerive :: Text -> Printer ()
printOpRecordDerive nm =
  pr $
    "$(makeFields ''"
      <> operatorNameToRecordName nm
      <> ")"

printOpRecord :: (Text, [(Text, Text)]) -> Printer ()
printOpRecord (mlilName, xs) = do
  pr $ "data " <> rname <> " expr = " <> rname
  indent $ case xs of
    [] -> pr derivingClause
    ((argName, argType) : args) -> do
      pr $ "{ _" <> pname <> capFirstText argName <> " :: " <> opTypeType argType
      printRestArgs pname args
  where
    rname = operatorNameToRecordName mlilName
    pname = operatorNameToPrefixName mlilName

---
printOpUnion :: Printer ()
printOpUnion = printOperationUnionType statementsData

printOpRecords :: Printer ()
printOpRecords = mapM_ (\x -> printOpRecord x >> pr "") statementsData

printDerives :: Printer ()
printDerives = mapM_ printOpRecordDerive (fst <$> statementsData)

printToFile :: FilePath -> IO ()
printToFile fp = TextIO.writeFile fp . Printer.toText $ do
  printRecordImports
  divide
  printOpUnion
  divide
  -- printOpRecords
  -- divide
  printDerives
  divide
  printBuilderCases
  where
    divide = br >> br >> br

printBuilderCase :: (Text, [(Text, Text)]) -> Printer ()
printBuilderCase (opName, []) =
  pr $
    "BN." <> opName <> " -> return " <> operatorNameToConstructorName opName
printBuilderCase (opName, args) = do
  pr $ "BN." <> opName <> " ->"
  indent . pr $
    "fmap " <> operatorNameToConstructorName opName <> " $ "
      <> operatorNameToRecordName opName
      <> " <$> "
      <> Text.intercalate " <*> " (opTypeBuilder . snd <$> args)

printBuilderCases :: Printer ()
printBuilderCases = indent $ mapM_ printBuilderCase statementsData

statementsData :: [(Text, [(Text, Text)])]
statementsData =
  [ ("MLIL_NOP", [])
  , ("MLIL_SET_VAR", [("dest", "var"), ("src", "expr")])
  , ("MLIL_SET_VAR_FIELD", [("dest", "var"), ("offset", "int"), ("src", "expr")])
  , ("MLIL_SET_VAR_SPLIT", [("high", "var"), ("low", "var"), ("src", "expr")])
  , ("MLIL_LOAD", [("src", "expr")])
  , ("MLIL_LOAD_STRUCT", [("src", "expr"), ("offset", "int")])
  , ("MLIL_STORE", [("dest", "expr"), ("src", "expr")])
  , ("MLIL_STORE_STRUCT", [("dest", "expr"), ("offset", "int"), ("src", "expr")])
  , ("MLIL_VAR", [("src", "var")])
  , ("MLIL_VAR_FIELD", [("src", "var"), ("offset", "int")])
  , ("MLIL_VAR_SPLIT", [("high", "var"), ("low", "var")])
  , ("MLIL_ADDRESS_OF", [("src", "var")])
  , ("MLIL_ADDRESS_OF_FIELD", [("src", "var"), ("offset", "int")])
  , ("MLIL_CONST", [("constant", "int")])
  , ("MLIL_CONST_DATA", [("constant", "int")])
  , ("MLIL_CONST_PTR", [("constant", "int")])
  , ("MLIL_EXTERN_PTR", [("constant", "int"), ("offset", "int")])
  , ("MLIL_FLOAT_CONST", [("constant", "float")])
  , ("MLIL_IMPORT", [("constant", "int")])
  , ("MLIL_ADD", [("left", "expr"), ("right", "expr")])
  , ("MLIL_ADC", [("left", "expr"), ("right", "expr"), ("carry", "expr")])
  , ("MLIL_SUB", [("left", "expr"), ("right", "expr")])
  , ("MLIL_SBB", [("left", "expr"), ("right", "expr"), ("carry", "expr")])
  , ("MLIL_AND", [("left", "expr"), ("right", "expr")])
  , ("MLIL_OR", [("left", "expr"), ("right", "expr")])
  , ("MLIL_XOR", [("left", "expr"), ("right", "expr")])
  , ("MLIL_LSL", [("left", "expr"), ("right", "expr")])
  , ("MLIL_LSR", [("left", "expr"), ("right", "expr")])
  , ("MLIL_ASR", [("left", "expr"), ("right", "expr")])
  , ("MLIL_ROL", [("left", "expr"), ("right", "expr")])
  , ("MLIL_RLC", [("left", "expr"), ("right", "expr"), ("carry", "expr")])
  , ("MLIL_ROR", [("left", "expr"), ("right", "expr")])
  , ("MLIL_RRC", [("left", "expr"), ("right", "expr"), ("carry", "expr")])
  , ("MLIL_MUL", [("left", "expr"), ("right", "expr")])
  , ("MLIL_MULU_DP", [("left", "expr"), ("right", "expr")])
  , ("MLIL_MULS_DP", [("left", "expr"), ("right", "expr")])
  , ("MLIL_DIVU", [("left", "expr"), ("right", "expr")])
  , ("MLIL_DIVU_DP", [("left", "expr"), ("right", "expr")])
  , ("MLIL_DIVS", [("left", "expr"), ("right", "expr")])
  , ("MLIL_DIVS_DP", [("left", "expr"), ("right", "expr")])
  , ("MLIL_MODU", [("left", "expr"), ("right", "expr")])
  , ("MLIL_MODU_DP", [("left", "expr"), ("right", "expr")])
  , ("MLIL_MODS", [("left", "expr"), ("right", "expr")])
  , ("MLIL_MODS_DP", [("left", "expr"), ("right", "expr")])
  , ("MLIL_NEG", [("src", "expr")])
  , ("MLIL_NOT", [("src", "expr")])
  , ("MLIL_SX", [("src", "expr")])
  , ("MLIL_ZX", [("src", "expr")])
  , ("MLIL_LOW_PART", [("src", "expr")])
  , ("MLIL_JUMP", [("dest", "expr")])
  , ("MLIL_JUMP_TO", [("dest", "expr"), ("targets", "address_list")])
  , ("MLIL_RET_HINT", [("dest", "expr")])
  , ("MLIL_CALL", [("output", "var_list"), ("dest", "expr"), ("params", "expr_list")])
  , ("MLIL_CALL_UNTYPED", [("output", "expr"), ("dest", "expr"), ("params", "expr"), ("stack", "expr")])
  , ("MLIL_CALL_OUTPUT", [("dest", "var_list")])
  , ("MLIL_CALL_PARAM", [("src", "var_list")])
  , ("MLIL_RET", [("src", "expr_list")])
  , ("MLIL_NORET", [])
  , ("MLIL_IF", [("condition", "expr"), ("true", "int"), ("false", "int")])
  , ("MLIL_GOTO", [("dest", "int")])
  , ("MLIL_CMP_E", [("left", "expr"), ("right", "expr")])
  , ("MLIL_CMP_NE", [("left", "expr"), ("right", "expr")])
  , ("MLIL_CMP_SLT", [("left", "expr"), ("right", "expr")])
  , ("MLIL_CMP_ULT", [("left", "expr"), ("right", "expr")])
  , ("MLIL_CMP_SLE", [("left", "expr"), ("right", "expr")])
  , ("MLIL_CMP_ULE", [("left", "expr"), ("right", "expr")])
  , ("MLIL_CMP_SGE", [("left", "expr"), ("right", "expr")])
  , ("MLIL_CMP_UGE", [("left", "expr"), ("right", "expr")])
  , ("MLIL_CMP_SGT", [("left", "expr"), ("right", "expr")])
  , ("MLIL_CMP_UGT", [("left", "expr"), ("right", "expr")])
  , ("MLIL_TEST_BIT", [("left", "expr"), ("right", "expr")])
  , ("MLIL_BOOL_TO_INT", [("src", "expr")])
  , ("MLIL_ADD_OVERFLOW", [("left", "expr"), ("right", "expr")])
  , ("MLIL_SYSCALL", [("output", "var_list"), ("params", "expr_list")])
  , ("MLIL_SYSCALL_UNTYPED", [("output", "expr"), ("params", "expr"), ("stack", "expr")])
  , ("MLIL_TAILCALL", [("output", "var_list"), ("dest", "expr"), ("params", "expr_list")])
  , ("MLIL_TAILCALL_UNTYPED", [("output", "expr"), ("dest", "expr"), ("params", "expr"), ("stack", "expr")])
  , ("MLIL_BP", [])
  , ("MLIL_TRAP", [("vector", "int")])
  , ("MLIL_INTRINSIC", [("output", "var_list"), ("intrinsic", "intrinsic"), ("params", "expr_list")])
  , ("MLIL_INTRINSIC_SSA", [("output", "var_ssa_list"), ("intrinsic", "intrinsic"), ("params", "expr_list")])
  , ("MLIL_FREE_VAR_SLOT", [("dest", "var")])
  , ("MLIL_FREE_VAR_SLOT_SSA", [("prev", "var_ssa_dest_and_src")])
  , ("MLIL_UNDEF", [])
  , ("MLIL_UNIMPL", [])
  , ("MLIL_UNIMPL_MEM", [("src", "expr")])
  , ("MLIL_FADD", [("left", "expr"), ("right", "expr")])
  , ("MLIL_FSUB", [("left", "expr"), ("right", "expr")])
  , ("MLIL_FMUL", [("left", "expr"), ("right", "expr")])
  , ("MLIL_FDIV", [("left", "expr"), ("right", "expr")])
  , ("MLIL_FSQRT", [("src", "expr")])
  , ("MLIL_FNEG", [("src", "expr")])
  , ("MLIL_FABS", [("src", "expr")])
  , ("MLIL_FLOAT_TO_INT", [("src", "expr")])
  , ("MLIL_INT_TO_FLOAT", [("src", "expr")])
  , ("MLIL_FLOAT_CONV", [("src", "expr")])
  , ("MLIL_ROUND_TO_INT", [("src", "expr")])
  , ("MLIL_FLOOR", [("src", "expr")])
  , ("MLIL_CEIL", [("src", "expr")])
  , ("MLIL_FTRUNC", [("src", "expr")])
  , ("MLIL_FCMP_E", [("left", "expr"), ("right", "expr")])
  , ("MLIL_FCMP_NE", [("left", "expr"), ("right", "expr")])
  , ("MLIL_FCMP_LT", [("left", "expr"), ("right", "expr")])
  , ("MLIL_FCMP_LE", [("left", "expr"), ("right", "expr")])
  , ("MLIL_FCMP_GE", [("left", "expr"), ("right", "expr")])
  , ("MLIL_FCMP_GT", [("left", "expr"), ("right", "expr")])
  , ("MLIL_FCMP_O", [("left", "expr"), ("right", "expr")])
  , ("MLIL_FCMP_UO", [("left", "expr"), ("right", "expr")])
  , ("MLIL_SET_VAR_SSA", [("dest", "var_ssa"), ("src", "expr")])
  , ("MLIL_SET_VAR_SSA_FIELD", [("prev", "var_ssa_dest_and_src"), ("offset", "int"), ("src", "expr")])
  , ("MLIL_SET_VAR_SPLIT_SSA", [("high", "var_ssa"), ("low", "var_ssa"), ("src", "expr")])
  , ("MLIL_SET_VAR_ALIASED", [("prev", "var_ssa_dest_and_src"), ("src", "expr")])
  , ("MLIL_SET_VAR_ALIASED_FIELD", [("prev", "var_ssa_dest_and_src"), ("offset", "int"), ("src", "expr")])
  , ("MLIL_VAR_SSA", [("src", "var_ssa")])
  , ("MLIL_VAR_SSA_FIELD", [("src", "var_ssa"), ("offset", "int")])
  , ("MLIL_VAR_ALIASED", [("src", "var_ssa")])
  , ("MLIL_VAR_ALIASED_FIELD", [("src", "var_ssa"), ("offset", "int")])
  , ("MLIL_VAR_SPLIT_SSA", [("high", "var_ssa"), ("low", "var_ssa")])
  , ("MLIL_CALL_SSA", [("output", "expr"), ("dest", "expr"), ("params", "expr_list"), ("src_memory", "int")])
  , ("MLIL_CALL_UNTYPED_SSA", [("output", "expr"), ("dest", "expr"), ("params", "expr"), ("stack", "expr")])
  , ("MLIL_SYSCALL_SSA", [("output", "expr"), ("params", "expr_list"), ("src_memory", "int")])
  , ("MLIL_SYSCALL_UNTYPED_SSA", [("output", "expr"), ("params", "expr"), ("stack", "expr")])
  , ("MLIL_TAILCALL_SSA", [("output", "expr"), ("dest", "expr"), ("params", "expr_list"), ("src_memory", "int")])
  , ("MLIL_TAILCALL_UNTYPED_SSA", [("output", "expr"), ("dest", "expr"), ("params", "expr"), ("stack", "expr")])
  , ("MLIL_CALL_OUTPUT_SSA", [("dest_memory", "int"), ("dest", "var_ssa_list")])
  , ("MLIL_CALL_PARAM_SSA", [("src_memory", "int"), ("src", "var_ssa_list")])
  , ("MLIL_LOAD_SSA", [("src", "expr"), ("src_memory", "int")])
  , ("MLIL_LOAD_STRUCT_SSA", [("src", "expr"), ("offset", "int"), ("src_memory", "int")])
  , ("MLIL_STORE_SSA", [("dest", "expr"), ("dest_memory", "int"), ("src_memory", "int"), ("src", "expr")])
  , ("MLIL_STORE_STRUCT_SSA", [("dest", "expr"), ("offset", "int"), ("dest_memory", "int"), ("src_memory", "int"), ("src", "expr")])
  , ("MLIL_VAR_PHI", [("dest", "var_ssa"), ("src", "var_ssa_list")])
  , ("MLIL_MEM_PHI", [("dest_memory", "int"), ("src_memory", "int_list")])
  ]

---------------

printRecordImport :: Text -> Printer ()
printRecordImport rname = pr $ "import Binja.Types.MLIL.Op." <> rname <> " as Exports ( " <> rname <> "(" <> rname <> ") )"

printRecordImports :: Printer ()
printRecordImports = mapM_ (printRecordImport . operatorNameToRecordName . fst) statementsData

printModuleHeader :: Text -> Printer ()
printModuleHeader name = do
  pr $ "module Binja.Types.MLIL.Op." <> name <> " where"
  br
  pr "import Binja.Prelude"

printModuleImports ::
  Set Text ->
  Printer ()
-- printModuleImports = mapM_ (maybe (return ()) pr . g . f) . Set.toList
printModuleImports types = mapM_ pr imports
  where
    imports :: [Text]
    imports = nub . mapMaybe identity . fmap (g . f) $ Set.toList types

    f :: Text -> Text
    f "var" = "Variable"
    f "var_list" = "Variable"
    f "var_ssa" = "SSAVariable"
    f "var_ssa_list" = "SSAVariable"
    f "var_ssa_dest_and_src" = "SSAVariableDestAndSrc"
    f "intrinsic" = "Intrinsic"
    f _ = ""

    g :: Text -> Maybe Text
    g "Variable" = Just "import Binja.Types.Variable (Variable)"
    g "SSAVariable" = Just "import Binja.Types.MLIL.Common (SSAVariable)"
    g "SSAVariableDestAndSrc" = Just "import Binja.Types.MLIL.Common (SSAVariableDestAndSrc)"
    g "Intrinsic" = Just "import Binja.Types.MLIL.Common (Intrinsic)"
    g _ = Nothing

printRecordModule :: (Text, [(Text, Text)]) -> Printer ()
printRecordModule (mlilName, xs) = do
  printModuleHeader rname
  br
  printModuleImports . Set.fromList . fmap snd $ xs
  br
  pr $ "data " <> rname <> " expr = " <> rname
  indent $ case xs of
    [] -> pr derivingClause
    ((argName, argType) : args) -> do
      pr $ "{ _" <> pname <> capFirstText argName <> " :: " <> opTypeType argType
      printRestArgs pname args
  br
  pr $ "instance Hashable a => Hashable (" <> rname <> " a)"
  where
    rname = operatorNameToRecordName mlilName
    pname = operatorNameToPrefixName mlilName

writeRecordModule :: FilePath -> (Text, [(Text, Text)]) -> IO ()
writeRecordModule outDir v@(mlilName, _) =
  TextIO.writeFile outModulePath $ (Printer.toText $ printRecordModule v) <> "\n"
  where
    outModulePath = outDir </> recordName <.> ".hs"
      where
        recordName = Text.unpack $ operatorNameToRecordName mlilName

writeRecords :: FilePath -> IO ()
writeRecords outDir = do
  putStr $ "Writing to " <> outDir <> "... "
  createDirectoryIfMissing True outDir
  mapM_ (writeRecordModule outDir) statementsData
  putStrLn ("done" :: String)
