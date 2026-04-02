module Flint.Shell.Commands.PrimDef
  ( primDefCommand
  , primListCommand
  , primRemoveCommand
  , primResetCommand
  ) where

import Flint.Prelude hiding (Location)

import Data.IORef

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import Flint.Types.Analysis.Path.Matcher
import Flint.Types.Analysis.Path.Matcher.Func (Func(..))
import Flint.Types.Analysis.Path.Matcher.Primitives (PrimSpec(..))
import Flint.Types.Symbol (Symbol(..))
import qualified Flint.Analysis.Path.Matcher.Primitives.Library as PrimLib

import qualified Blaze.Types.Pil as Pil

import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

-- ===================================================================
-- Expression Pattern Parser
-- ===================================================================

-- | Parse an expression pattern from text.
--
-- Syntax:
--   @name           → Bound "name"
--   _               → Wild
--   name            → Bind "name" Wild
--   name[tainted]   → Bind "name" (Contains (TaintedBy Wild))
--   name[tainted:global] → Bind "name" (Contains (TaintedBy GlobalAddr))
--   name[param]     → Bind "name" (Contains Param)
--   name[global]    → Bind "name" (Contains GlobalAddr)
--   name[!immediate] → Bind "name" (NotPattern Immediate)
--   (load @name)    → Expr (LOAD (Bound "name"))
--   (add e1 e2)     → Expr (ADD e1 e2)
--   (contains e)    → Contains e
--   (not e)         → NotPattern e
parseExprPat :: Text -> Either Text ExprPattern
parseExprPat t
  | Text.null stripped = Left "Empty expression"
  | stripped == "_" = Right Wild
  | Just rest <- Text.stripPrefix "@" stripped
  = Right $ Bound (coerce rest)
  | Just inner <- unwrapParens stripped
  = parseCompoundExpr inner
  | Just (name, modifier) <- parseBracketModifier stripped
  = Right $ Bind (coerce name) modifier
  | isImmediateToken stripped
  = Right Immediate
  | otherwise
  = Right $ Bind (coerce stripped) Wild
  where
    stripped = Text.strip t

isImmediateToken :: Text -> Bool
isImmediateToken t =
  isJust (readMaybe (Text.unpack t) :: Maybe Integer)
  || isHexLiteral t
  where
    isHexLiteral s = case Text.stripPrefix "0x" s <|> Text.stripPrefix "0X" s of
      Just digits -> not (Text.null digits) && Text.all isHexDigit digits
      Nothing -> False

-- | Parse "[modifier]" from "name[modifier]"
parseBracketModifier :: Text -> Maybe (Text, ExprPattern)
parseBracketModifier t = do
  let (name, rest) = Text.breakOn "[" t
  guard (not $ Text.null rest)
  guard (Text.isSuffixOf "]" rest)
  let inner = Text.drop 1 $ Text.dropEnd 1 rest
  pat <- case inner of
    "tainted:global" -> Just $ Contains (TaintedBy GlobalAddr)
    "tainted"        -> Just $ Contains (TaintedBy Wild)
    "param"          -> Just $ Contains Param
    "global"         -> Just $ Contains GlobalAddr
    "!immediate"     -> Just $ NotPattern Immediate
    _                -> Nothing
  return (name, pat)

-- | Strip outer parens: "(foo bar)" → Just "foo bar"
unwrapParens :: Text -> Maybe Text
unwrapParens t
  | Text.isPrefixOf "(" t && Text.isSuffixOf ")" t
  = Just . Text.strip . Text.drop 1 $ Text.dropEnd 1 t
  | otherwise = Nothing

data Nesting = Nesting
  { parenDepth :: Int
  , bracketDepth :: Int
  , braceDepth :: Int
  }

emptyNesting :: Nesting
emptyNesting = Nesting 0 0 0

updateNesting :: Nesting -> Char -> Nesting
updateNesting nesting = \case
  '(' -> nesting { parenDepth = parenDepth nesting + 1 }
  ')' -> nesting { parenDepth = max 0 (parenDepth nesting - 1) }
  '[' -> nesting { bracketDepth = bracketDepth nesting + 1 }
  ']' -> nesting { bracketDepth = max 0 (bracketDepth nesting - 1) }
  '{' -> nesting { braceDepth = braceDepth nesting + 1 }
  '}' -> nesting { braceDepth = max 0 (braceDepth nesting - 1) }
  _ -> nesting

isTopLevel :: Nesting -> Bool
isTopLevel nesting =
  parenDepth nesting == 0
  && bracketDepth nesting == 0
  && braceDepth nesting == 0

splitTopLevel :: (Char -> Bool) -> Text -> [Text]
splitTopLevel isDelimiter =
  filter (not . Text.null)
  . fmap Text.strip
  . reverse
  . go emptyNesting [] []
  . Text.unpack
  where
    go :: Nesting -> String -> [Text] -> String -> [Text]
    go _ acc result [] = Text.pack (reverse acc) : result
    go nesting acc result (c : rest)
      | isTopLevel nesting && isDelimiter c =
          go nesting [] (Text.pack (reverse acc) : result) rest
      | otherwise =
          let nesting' = updateNesting nesting c
          in go nesting' (c : acc) result rest

splitTopLevelWords :: Text -> [Text]
splitTopLevelWords = splitTopLevel isSpace

splitTwo :: Text -> Maybe (Text, Text)
splitTwo t = case splitTopLevelWords t of
  (a : rest@(_ : _)) -> Just (a, Text.unwords rest)
  _ -> Nothing

splitTrailingParenGroup :: Text -> Maybe (Text, Text)
splitTrailingParenGroup t = do
  let stripped = Text.strip t
      chars = Text.unpack stripped
  guard (not $ null chars)
  guard (Text.isSuffixOf ")" stripped)
  openIdx <- findMatchingOpen chars
  let (funcText, argsWithParens) = splitAt openIdx chars
      argsText = take (length argsWithParens - 2) (drop 1 argsWithParens)
  guard (not $ null funcText)
  return (Text.strip $ Text.pack funcText, Text.pack argsText)
  where
    findMatchingOpen :: String -> Maybe Int
    findMatchingOpen chars =
      go (0 :: Int) . reverse $ zip [0 ..] chars
      where
        go _ [] = Nothing
        go depth ((idx, c) : rest) = case c of
          ')' -> go (depth + 1) rest
          '(' ->
            let depth' = depth - 1
            in if depth' == 0 then Just idx else go depth' rest
          _ -> go depth rest

-- | Parse compound expressions: (load @x), (add e1 e2), (contains e), (not e)
parseCompoundExpr :: Text -> Either Text ExprPattern
parseCompoundExpr t = case splitTopLevelWords t of
  ["load", arg] -> do
    e <- parseExprPat arg
    Right $ Expr (Pil.LOAD $ Pil.LoadOp e)
  ["add", a, b] -> do
    e1 <- parseExprPat a
    e2 <- parseExprPat b
    Right $ Expr (Pil.ADD $ Pil.AddOp e1 e2)
  ["contains", arg] -> Contains <$> parseExprPat arg
  ["not", arg] -> NotPattern <$> parseExprPat arg
  _ -> Left $ "Unknown compound expression: (" <> t <> ")"

-- ===================================================================
-- Call Destination Parser
-- ===================================================================

-- | Parse a call destination and its args from "funcname(arg1, arg2, ...)"
-- Also handles "{name1, name2}(args)" for FuncNames.
parseCallExpr :: Text -> Either Text (CallDest ExprPattern, [ExprPattern], Maybe ExprPattern)
parseCallExpr t = do
  (funcText, argsStr) <- maybe
    (Left $ "Expected trailing argument list in call: " <> t)
    Right
    (splitTrailingParenGroup t)
  args <- traverse parseExprPat (splitArgs argsStr)
  dest <- parseCallDest funcText
  return (dest, args, Nothing)

-- | Parse function destination: "name", "{name1, name2}", "indirect(expr)"
parseCallDest :: Text -> Either Text (CallDest ExprPattern)
parseCallDest t
  | Just inner <- Text.stripPrefix "indirect(" =<< (if Text.isSuffixOf ")" t then Just t else Nothing)
  = do
      let exprText = Text.dropEnd 1 inner
      CallIndirect <$> parseExprPat exprText
  | Text.isPrefixOf "{" t && Text.isSuffixOf "}" t
  = do
      let inner = Text.drop 1 $ Text.dropEnd 1 t
          names = Text.strip <$> Text.splitOn "," inner
      Right . CallFunc . FuncNames $ HashSet.fromList names
  | otherwise
  = Right . CallFunc $ FuncName t

-- | Split comma-separated args, respecting parens nesting.
splitArgs :: Text -> [Text]
splitArgs = splitTopLevel (== ',')

-- ===================================================================
-- Statement Pattern Parser
-- ===================================================================

-- | Parse a single statement pattern line.
--
-- Handles optional "label:" prefix.
-- Keywords: *, def, store, call, ret, constraint, avoid_until, end
parseStmtLine :: Text -> Either Text StmtPattern
parseStmtLine t
  | stripped == "*" = Right Star
  | stripped == "end" = Right EndOfPath
  | otherwise = case parseStmtBody stripped of
      Right pat -> Right pat
      Left bodyErr -> case extractLabel stripped of
        Just (label, rest) -> do
          pat <- parseStmtBody rest
          Right $ Location (coerce label) pat
        Nothing -> Left bodyErr
  where
    stripped = Text.strip t

-- | Extract "label: rest" → Just (label, rest), but not "def", "call", etc.
extractLabel :: Text -> Maybe (Text, Text)
extractLabel t = do
  let parts = splitTopLevel (== ':') t
  (label, restParts) <- case parts of
    x : xs@(_ : _) -> Just (Text.strip x, xs)
    _ -> Nothing
  let rest = Text.intercalate ":" restParts
  -- Don't treat keywords as labels
  guard (label `notElem` ["def", "store", "call", "ret", "constraint", "avoid_until", "end"])
  -- Don't treat "indirect" or things with parens as labels
  guard (not $ Text.isInfixOf "(" label)
  guard (not $ Text.any isSpace label)
  return (label, rest)

-- | Parse the body of a statement pattern (after optional label).
parseStmtBody :: Text -> Either Text StmtPattern
parseStmtBody t
  | Just rest <- Text.stripPrefix "def " t = parseDef rest
  | Just rest <- Text.stripPrefix "store " t = parseStore rest
  | Just rest <- Text.stripPrefix "call " t = parseCall rest
  | Just rest <- Text.stripPrefix "ret " t = parseRet rest
  | Just rest <- Text.stripPrefix "constraint " t = parseConstraint rest
  | Just rest <- Text.stripPrefix "avoid_until(" t = parseAvoidUntil rest
  | otherwise = Left $ "Unknown statement pattern: " <> t

-- | Parse "def dst = call func(args)" or "def dst = expr"
parseDef :: Text -> Either Text StmtPattern
parseDef t = do
  let (dstText, rest) = Text.breakOn "=" t
  when (Text.null rest) $
    Left $ "Expected '=' in def: " <> t
  let src = Text.strip $ Text.drop 1 rest
      dst = Text.strip dstText
  dstPat <- parseExprPat dst
  if "call " `Text.isPrefixOf` src
    then do
      let callText = Text.drop 5 src
      (dest, args, _) <- parseCallExpr callText
      Right . Stmt $ Call (Just dstPat) dest args
    else do
      srcPat <- parseExprPat src
      Right . Stmt $ Def dstPat srcPat

-- | Parse "store dst_expr val_expr"
parseStore :: Text -> Either Text StmtPattern
parseStore t = do
  (a, b) <- maybe (Left $ "Expected store <dst> <value> in: " <> t) Right (splitTwo t)
  dstPat <- parseExprPat a
  valPat <- parseExprPat b
  Right . Stmt $ Store dstPat valPat

-- | Parse "call func(args)" — standalone call with no return value
parseCall :: Text -> Either Text StmtPattern
parseCall t = do
  (dest, args, _) <- parseCallExpr t
  Right . Stmt $ Call Nothing dest args

-- | Parse "ret expr"
parseRet :: Text -> Either Text StmtPattern
parseRet t = do
  pat <- parseExprPat t
  Right . Stmt $ Ret pat

-- | Parse "constraint expr"
parseConstraint :: Text -> Either Text StmtPattern
parseConstraint t = do
  pat <- parseExprPat t
  Right . Stmt $ Constraint pat

-- | Parse "avoid_until(!avoid_pat, until_pat)"
-- The closing ")" was already stripped by the caller check, but we need to handle it.
parseAvoidUntil :: Text -> Either Text StmtPattern
parseAvoidUntil t = do
  -- t is everything after "avoid_until(" — find matching close paren
  let body = if Text.isSuffixOf ")" t then Text.dropEnd 1 t else t
  -- Split on the comma separating avoid from until
  -- The avoid starts with "!"
  case Text.stripPrefix "!" body of
    Nothing -> Left "avoid_until expects !<avoid_pattern> as first argument"
    Just rest -> do
      let (avoidText, untilText) = splitAvoidUntil rest
      avoidPat <- parseStmtLine (Text.strip avoidText)
      untilPat <- parseStmtLine (Text.strip untilText)
      Right . AvoidUntil $ AvoidSpec
        { avoid = avoidPat
        , until = untilPat
        }

-- | Split "avoid_body, until_body" respecting parens
splitAvoidUntil :: Text -> (Text, Text)
splitAvoidUntil t = case splitTopLevel (== ',') t of
  (avoidText : untilText : rest) ->
    (avoidText, Text.intercalate ", " (untilText : rest))
  [avoidText] -> (avoidText, "")
  [] -> ("", "")

-- ===================================================================
-- Top-level prim-def parser
-- ===================================================================

-- | Parse "prim-def Name { line1 \n line2 \n ... }"
-- The args come in as shell words, so we rejoin them and split on newlines/semicolons.
parsePrimDef :: Text -> [Text] -> Either Text Prim
parsePrimDef name args = do
  let joined = Text.intercalate " " args
  -- Strip { } braces
  body <- case (Text.stripPrefix "{" (Text.strip joined), Text.isSuffixOf "}" (Text.strip joined)) of
    (Just rest, True) -> Right . Text.strip $ Text.dropEnd 1 rest
    _ -> Left "Expected { ... } block. Usage: prim-def Name { * \\n stmt1 \\n * \\n stmt2 }"
  -- Split into lines (newlines or semicolons)
  let rawLines = concatMap (Text.splitOn ";") $ Text.lines body
      nonEmptyLines = filter (not . Text.null . Text.strip)
                    . fmap Text.strip
                    $ rawLines
  when (null nonEmptyLines) $
    Left "Empty pattern block"
  stmtPats <- traverse parseStmtLine nonEmptyLines
  let pat = ordered stmtPats
  Right $ buildPrim name pat

-- | Build a Prim from name and pattern, extracting var/location names.
buildPrim :: Text -> StmtPattern -> Prim
buildPrim name pat = Prim
  { primType = PrimSpec
      { name = name
      , vars = HashSet.fromList $ extractBindNames pat
      , locations = HashSet.fromList $ extractLocNames pat
      }
  , stmtPattern = pat
  }

-- | Extract all Bind symbol names from a StmtPattern tree.
extractBindNames :: StmtPattern -> [Symbol expr]
extractBindNames = \case
  And a b -> extractBindNames a <> extractBindNames b
  Or a b -> extractBindNames a <> extractBindNames b
  Location _ p -> extractBindNames p
  Stmt s -> extractBindNamesStmt s
  -- `avoid` is checked negatively after `until` matches, so only bindings
  -- from the `until` side are guaranteed to exist on a successful match.
  AvoidUntil (AvoidSpec _ u) -> extractBindNames u
  _ -> []
  where
    extractBindNamesStmt :: Statement ExprPattern -> [Symbol expr]
    extractBindNamesStmt = \case
      Def a b -> extractBindNamesExpr a <> extractBindNamesExpr b
      Store a b -> extractBindNamesExpr a <> extractBindNamesExpr b
      Call mr dest args ->
        maybe [] extractBindNamesExpr mr
        <> extractBindNamesCallDest dest
        <> concatMap extractBindNamesExpr args
      Ret e -> extractBindNamesExpr e
      Constraint e -> extractBindNamesExpr e
      _ -> []

    extractBindNamesCallDest :: CallDest ExprPattern -> [Symbol expr]
    extractBindNamesCallDest = \case
      CallFunc _ -> []
      CallIndirect e -> extractBindNamesExpr e

    extractBindNamesExpr :: ExprPattern -> [Symbol expr]
    extractBindNamesExpr = \case
      Expr op -> foldMap extractBindNamesExpr op
      Bind s p -> [coerce s] <> extractBindNamesExpr p
      BindWidth s p -> [coerce s] <> extractBindNamesExpr p
      Contains p -> extractBindNamesExpr p
      TaintedBy p -> extractBindNamesExpr p
      Cmp _ a b -> extractBindNamesExpr a <> extractBindNamesExpr b
      NotPattern p -> extractBindNamesExpr p
      OrPattern a b -> extractBindNamesExpr a <> extractBindNamesExpr b
      OfType _ p -> extractBindNamesExpr p
      _ -> []

-- | Extract all Location symbol names from a StmtPattern tree.
extractLocNames :: StmtPattern -> [Symbol addr]
extractLocNames = \case
  And a b -> extractLocNames a <> extractLocNames b
  Or a b -> extractLocNames a <> extractLocNames b
  Location s p -> [coerce s] <> extractLocNames p
  -- Locations from the negative `avoid` branch are not part of the matched
  -- primitive; only `until` locations are stable outputs.
  AvoidUntil (AvoidSpec _ u) -> extractLocNames u
  _ -> []

-- ===================================================================
-- Shell Commands
-- ===================================================================

primDefCommand :: ShellCommand
primDefCommand = ShellCommand
  { cmdName = "prim-def"
  , cmdAliases = ["pd"]
  , cmdHelp = "Define a dynamic WMI pattern"
  , cmdUsage = "prim-def <name> { <pattern lines separated by ; or newlines> }"
  , cmdAction = primDefAction
  }

primDefAction :: ShellState -> [Text] -> IO CommandResult
primDefAction _st [] = return $ ResultError $ Text.unlines
  [ "Usage: prim-def <name> { <pattern> }"
  , ""
  , "Example:"
  , "  prim-def MyOverflow { * ; alloc: def p = call malloc(sz[tainted:global]) ; * ; copy: call memcpy(@p, src, len) }"
  , ""
  , "Statement patterns:"
  , "  *                          — match any statements (Star)"
  , "  def dst = expr             — variable definition"
  , "  def dst = call func(args)  — call with return value"
  , "  call func(args)            — call without return"
  , "  store dst val              — memory store"
  , "  ret expr                   — return"
  , "  end                        — end of path"
  , "  avoid_until(!avoid, until) — negative constraint"
  , ""
  , "Expression patterns:"
  , "  name             — bind to name (Wild)"
  , "  name[tainted]    — bind, must be tainted"
  , "  name[tainted:global] — bind, tainted by global"
  , "  @name            — reference previous binding"
  , "  _                — wildcard"
  , "  (load @name)     — memory load"
  , "  (add e1 e2)      — addition"
  , ""
  , "Labels: prefix any stmt with 'label:' for location tracking"
  ]
primDefAction _st [_] = return $ ResultError "Missing pattern block. Use: prim-def <name> { ... }"
primDefAction st (name : rest) = do
  case parsePrimDef name rest of
    Left err -> return $ ResultError $ "Parse error: " <> err
    Right prim -> do
      allPrims <- getAllPrims st
      let duplicateExists =
            any (\p -> Text.toLower (p ^. #primType . #name) == Text.toLower name) allPrims
      if duplicateExists
        then return $ ResultError $
          "Pattern name already exists: " <> name <> ". Choose a different name."
        else do
          modifyIORef' (st ^. #userPrims) (<> [prim])
          let spec = prim ^. #primType
              vars = Text.intercalate ", " . fmap (view #unSymbol) . HashSet.toList $ spec ^. #vars
              locs = Text.intercalate ", " . fmap (view #unSymbol) . HashSet.toList $ spec ^. #locations
          return $ ResultOk $ "Defined pattern: " <> name
            <> "  vars: {" <> vars <> "}"
            <> "  locations: {" <> locs <> "}"

-- ---------------------------------------------------------------------------

primListCommand :: ShellCommand
primListCommand = ShellCommand
  { cmdName = "prim-list"
  , cmdAliases = ["pl"]
  , cmdHelp = "List all WMI primitives (built-in + user)"
  , cmdUsage = "prim-list"
  , cmdAction = primListAction
  }

primListAction :: ShellState -> [Text] -> IO CommandResult
primListAction st _args = do
  user <- readIORef (st ^. #userPrims)
  let builtinLines = fmap (\p -> formatPrimEntry p <> "  (built-in)") PrimLib.allPrims
      userLines = fmap formatPrimEntry user
  return $ ResultText $ Text.unlines $ builtinLines <> userLines

formatPrimEntry :: Prim -> Text
formatPrimEntry p =
  let spec = p ^. #primType
      n = spec ^. #name
      vars = Text.intercalate ", " . fmap (view #unSymbol) . HashSet.toList $ spec ^. #vars
      locs = Text.intercalate ", " . fmap (view #unSymbol) . HashSet.toList $ spec ^. #locations
  in "  " <> n <> "  vars: {" <> vars <> "}  locations: {" <> locs <> "}"

-- ---------------------------------------------------------------------------

primRemoveCommand :: ShellCommand
primRemoveCommand = ShellCommand
  { cmdName = "prim-remove"
  , cmdAliases = ["prm"]
  , cmdHelp = "Remove a user-defined WMI pattern"
  , cmdUsage = "prim-remove <name>"
  , cmdAction = primRemoveAction
  }

primRemoveAction :: ShellState -> [Text] -> IO CommandResult
primRemoveAction _st [] = return $ ResultError "Usage: prim-remove <name>"
primRemoveAction st (name : _) = do
  old <- readIORef (st ^. #userPrims)
  let new = filter (\p -> p ^. #primType . #name /= name) old
      removed = length old - length new
  writeIORef (st ^. #userPrims) new
  if removed > 0
    then return $ ResultOk $ "Removed pattern: " <> name
    else return $ ResultError $ "No user-defined pattern found: " <> name

-- ---------------------------------------------------------------------------

primResetCommand :: ShellCommand
primResetCommand = ShellCommand
  { cmdName = "prim-reset"
  , cmdAliases = []
  , cmdHelp = "Clear all user-defined WMI patterns"
  , cmdUsage = "prim-reset"
  , cmdAction = primResetAction
  }

primResetAction :: ShellState -> [Text] -> IO CommandResult
primResetAction st _args = do
  writeIORef (st ^. #userPrims) []
  return $ ResultOk "Cleared all user-defined patterns."
