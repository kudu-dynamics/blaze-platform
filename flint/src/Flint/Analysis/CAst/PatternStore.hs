-- | Dynamic creation, storage, save/load for C AST vulnerability patterns.
--
-- Lets users (or Claude via MCP) define new CAstCheck patterns at runtime
-- using a JSON representation, manage them in memory, and persist to/from files.
--
-- Orphan instances: JSON instances for CAst pattern types (Severity, CExprPat,
-- CStmtPat, CAstCheck) live here because this module owns JSON de/serialization
-- and the underlying types live in non-JSON modules we don't want coupled to
-- aeson.
{-# OPTIONS_GHC -Wno-orphans #-}
module Flint.Analysis.CAst.PatternStore
  ( -- * JSON serialization
    exprPatFromJSON
  , exprPatToJSON
  , stmtPatFromJSON
  , stmtPatToJSON
    -- * Store operations
  , getUserChecks
  , addUserCheck
  , removeUserCheck
  , listUserChecks
  , clearUserChecks
  , saveUserChecks
  , loadUserChecks
    -- * Combined checks (built-in + user)
  , getAllChecks
  ) where

import Flint.Prelude

import Flint.Types.Analysis.CAst.Matcher
import Flint.Analysis.CAst.Patterns (allChecks)

import Data.Aeson
  ( FromJSON(..), ToJSON(..)
  , Value(..), object, withObject, withText
  , (.:), (.:?)
  , eitherDecodeStrict
  )
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, parseFail)
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text


-- ---------------------------------------------------------------------------
-- JSON instances for Severity
-- ---------------------------------------------------------------------------

instance ToJSON Severity where
  toJSON = \case
    Info     -> String "info"
    Low      -> String "low"
    Medium   -> String "medium"
    High     -> String "high"
    Critical -> String "critical"

instance FromJSON Severity where
  parseJSON = withText "Severity" $ \t -> case Text.toLower t of
    "info"     -> pure Info
    "low"      -> pure Low
    "medium"   -> pure Medium
    "high"     -> pure High
    "critical" -> pure Critical
    _          -> parseFail $ "Unknown severity: " <> Text.unpack t


-- ---------------------------------------------------------------------------
-- JSON instances for CExprPat
-- ---------------------------------------------------------------------------

instance ToJSON CExprPat where
  toJSON = exprPatToJSON

instance FromJSON CExprPat where
  parseJSON = exprPatFromJSON

exprPatToJSON :: CExprPat -> Value
exprPatToJSON = \case
  CWild -> object ["wild" Aeson..=True]
  CBind name pat -> object ["bind" Aeson..=name, "pat" Aeson..=exprPatToJSON pat]
  CBound name -> object ["bound" Aeson..=name]
  CUses name -> object ["uses" Aeson..=name]
  CNamePat name -> object ["name" Aeson..=name]
  CNameRegex rx -> object ["nameRegex" Aeson..=rx]
  CLitIntPat -> object ["litInt" Aeson..=True]
  CLitStringPat -> object ["litString" Aeson..=True]
  CCallPat name args -> object ["call" Aeson..=name, "args" Aeson..=fmap exprPatToJSON args]
  CCallNamesPat names args -> object
    ["callNames" Aeson..=HashSet.toList names, "args" Aeson..=fmap exprPatToJSON args]
  CIndexPat arr idx -> object ["index" Aeson..=exprPatToJSON arr, "idx" Aeson..=exprPatToJSON idx]
  CArrowPat expr fld -> object ["arrow" Aeson..=exprPatToJSON expr, "field" Aeson..=fld]
  CDotPat expr fld -> object ["dot" Aeson..=exprPatToJSON expr, "field" Aeson..=fld]
  CBinOpPat op l r -> object ["binOp" Aeson..=op, "lhs" Aeson..=exprPatToJSON l, "rhs" Aeson..=exprPatToJSON r]
  CAnyCmpPat l r -> object ["anyCmp" Aeson..=[exprPatToJSON l, exprPatToJSON r]]
  CAssignPat l r -> object ["assign" Aeson..=exprPatToJSON l, "rhs" Aeson..=exprPatToJSON r]
  CUnaryPat op e -> object ["unaryOp" Aeson..=op, "expr" Aeson..=exprPatToJSON e]
  CCastPat ty e -> object ["cast" Aeson..=ty, "expr" Aeson..=exprPatToJSON e]
  CContains pat -> object ["contains" Aeson..=exprPatToJSON pat]
  CNotPat pat -> object ["not" Aeson..=exprPatToJSON pat]
  COrPat a b -> object ["or" Aeson..=[exprPatToJSON a, exprPatToJSON b]]
  CAndPat a b -> object ["and" Aeson..=[exprPatToJSON a, exprPatToJSON b]]

exprPatFromJSON :: Value -> Parser CExprPat
exprPatFromJSON = withObject "CExprPat" $ \o -> do
  -- Try each constructor in order
  let tryWild = (o .: "wild" :: Parser Bool) >> pure CWild
      tryBind = CBind <$> o .: "bind" <*> (o .: "pat" >>= exprPatFromJSON)
      tryBound = CBound <$> o .: "bound"
      tryUses = CUses <$> o .: "uses"
      tryName = CNamePat <$> o .: "name"
      tryNameRegex = CNameRegex <$> o .: "nameRegex"
      tryLitInt = (o .: "litInt" :: Parser Bool) >> pure CLitIntPat
      tryLitString = (o .: "litString" :: Parser Bool) >> pure CLitStringPat
      tryCall = CCallPat <$> o .: "call" <*> (o .: "args" >>= traverse exprPatFromJSON)
      tryCallNames = do
        names <- o .: "callNames"
        args <- o .:? "args" >>= \case
          Nothing -> pure []
          Just xs -> traverse exprPatFromJSON xs
        pure $ CCallNamesPat (HashSet.fromList names) args
      tryIndex = CIndexPat <$> (o .: "index" >>= exprPatFromJSON) <*> (o .: "idx" >>= exprPatFromJSON)
      tryArrow = CArrowPat <$> (o .: "arrow" >>= exprPatFromJSON) <*> o .: "field"
      tryDot = CDotPat <$> (o .: "dot" >>= exprPatFromJSON) <*> o .: "field"
      tryBinOp = CBinOpPat <$> o .: "binOp" <*> (o .: "lhs" >>= exprPatFromJSON) <*> (o .: "rhs" >>= exprPatFromJSON)
      tryAnyCmp = do
        xs <- o .: "anyCmp"
        case xs of
          [a, b] -> CAnyCmpPat <$> exprPatFromJSON a <*> exprPatFromJSON b
          _ -> parseFail "anyCmp requires exactly 2 elements"
      tryAssign = CAssignPat <$> (o .: "assign" >>= exprPatFromJSON) <*> (o .: "rhs" >>= exprPatFromJSON)
      tryUnary = CUnaryPat <$> o .: "unaryOp" <*> (o .: "expr" >>= exprPatFromJSON)
      tryCast = CCastPat <$> o .: "cast" <*> (o .: "expr" >>= exprPatFromJSON)
      tryContains = CContains <$> (o .: "contains" >>= exprPatFromJSON)
      tryNot = CNotPat <$> (o .: "not" >>= exprPatFromJSON)
      tryOr = do
        xs <- o .: "or"
        case xs of
          [a, b] -> COrPat <$> exprPatFromJSON a <*> exprPatFromJSON b
          _ -> parseFail "or requires exactly 2 elements"
      tryAnd = do
        xs <- o .: "and"
        case xs of
          [a, b] -> CAndPat <$> exprPatFromJSON a <*> exprPatFromJSON b
          _ -> parseFail "and requires exactly 2 elements"
  tryWild <|> tryBind <|> tryBound <|> tryUses <|> tryName <|> tryNameRegex
    <|> tryLitInt <|> tryLitString <|> tryCall <|> tryCallNames
    <|> tryIndex <|> tryArrow <|> tryDot <|> tryBinOp <|> tryAnyCmp
    <|> tryAssign <|> tryUnary <|> tryCast <|> tryContains <|> tryNot
    <|> tryOr <|> tryAnd


-- ---------------------------------------------------------------------------
-- JSON instances for CStmtPat
-- ---------------------------------------------------------------------------

instance ToJSON CStmtPat where
  toJSON = stmtPatToJSON

instance FromJSON CStmtPat where
  parseJSON = stmtPatFromJSON

stmtPatToJSON :: CStmtPat -> Value
stmtPatToJSON = \case
  CStmtExprPat e -> object ["expr" Aeson..=exprPatToJSON e]
  CVarDeclPat ty name mInit -> object $
    ["varDecl" Aeson..=ty, "varName" Aeson..=name]
    <> maybe [] (\i -> ["init" Aeson..=exprPatToJSON i]) mInit
  CIfPat cond body -> object ["if" Aeson..=exprPatToJSON cond, "body" Aeson..=fmap stmtPatToJSON body]
  CForPat fp -> object
    [ "for" Aeson..=object
        ( maybe [] (\p -> ["init" Aeson..=exprPatToJSON p]) (fp ^. #initPat)
        <> maybe [] (\p -> ["cond" Aeson..=exprPatToJSON p]) (fp ^. #condPat)
        <> maybe [] (\p -> ["incr" Aeson..=exprPatToJSON p]) (fp ^. #incrPat)
        <> ["body" Aeson..=fmap stmtPatToJSON (fp ^. #bodyPat)]
        )
    ]
  CWhilePat cond body -> object ["while" Aeson..=exprPatToJSON cond, "body" Aeson..=fmap stmtPatToJSON body]
  CDoWhilePat body cond -> object ["doWhile" Aeson..=fmap stmtPatToJSON body, "cond" Aeson..=exprPatToJSON cond]
  CAnyLoopPat body -> object ["anyLoop" Aeson..=fmap stmtPatToJSON body]
  CReturnPat e -> object ["return" Aeson..=exprPatToJSON e]
  CStar -> object ["star" Aeson..=True]
  CSequence pats -> object ["seq" Aeson..=fmap stmtPatToJSON pats]
  CBodyContains pat -> object ["bodyContains" Aeson..=stmtPatToJSON pat]
  CStmtOr a b -> object ["stmtOr" Aeson..=[stmtPatToJSON a, stmtPatToJSON b]]
  CStmtNot pat -> object ["stmtNot" Aeson..=stmtPatToJSON pat]
  CAvoidUntil avoid until_ -> object
    ["avoidUntil" Aeson..=object ["avoid" Aeson..=stmtPatToJSON avoid, "until" Aeson..=stmtPatToJSON until_]]

stmtPatFromJSON :: Value -> Parser CStmtPat
stmtPatFromJSON = withObject "CStmtPat" $ \o -> do
  let tryExpr = CStmtExprPat <$> (o .: "expr" >>= exprPatFromJSON)
      tryVarDecl = do
        ty <- o .: "varDecl"
        name <- o .: "varName"
        mInit <- o .:? "init" >>= traverse exprPatFromJSON
        pure $ CVarDeclPat ty name mInit
      tryIf = CIfPat <$> (o .: "if" >>= exprPatFromJSON) <*> (o .: "body" >>= traverse stmtPatFromJSON)
      tryFor = do
        fobj <- o .: "for"
        flip (withObject "ForPat") fobj $ \fo -> do
          initP <- fo .:? "init" >>= traverse exprPatFromJSON
          condP <- fo .:? "cond" >>= traverse exprPatFromJSON
          incrP <- fo .:? "incr" >>= traverse exprPatFromJSON
          bodyP <- fo .:? "body" >>= \case
            Nothing -> pure []
            Just xs -> traverse stmtPatFromJSON xs
          pure $ CForPat ForPat
            { initPat = initP, condPat = condP, incrPat = incrP, bodyPat = bodyP }
      tryWhile = CWhilePat <$> (o .: "while" >>= exprPatFromJSON) <*> (o .: "body" >>= traverse stmtPatFromJSON)
      tryDoWhile = CDoWhilePat <$> (o .: "doWhile" >>= traverse stmtPatFromJSON) <*> (o .: "cond" >>= exprPatFromJSON)
      tryAnyLoop = CAnyLoopPat <$> (o .: "anyLoop" >>= traverse stmtPatFromJSON)
      tryReturn = CReturnPat <$> (o .: "return" >>= exprPatFromJSON)
      tryStar = (o .: "star" :: Parser Bool) >> pure CStar
      trySeq = CSequence <$> (o .: "seq" >>= traverse stmtPatFromJSON)
      tryBodyContains = CBodyContains <$> (o .: "bodyContains" >>= stmtPatFromJSON)
      tryStmtOr = do
        xs <- o .: "stmtOr"
        case xs of
          [a, b] -> CStmtOr <$> stmtPatFromJSON a <*> stmtPatFromJSON b
          _ -> parseFail "stmtOr requires exactly 2 elements"
      tryStmtNot = CStmtNot <$> (o .: "stmtNot" >>= stmtPatFromJSON)
      tryAvoidUntil = do
        ao <- o .: "avoidUntil"
        flip (withObject "AvoidUntil") ao $ \au ->
          CAvoidUntil <$> (au .: "avoid" >>= stmtPatFromJSON) <*> (au .: "until" >>= stmtPatFromJSON)
  tryExpr <|> tryVarDecl <|> tryIf <|> tryFor <|> tryWhile <|> tryDoWhile
    <|> tryAnyLoop <|> tryReturn <|> tryStar <|> trySeq <|> tryBodyContains
    <|> tryStmtOr <|> tryStmtNot <|> tryAvoidUntil


-- ---------------------------------------------------------------------------
-- JSON instances for CAstCheck
-- ---------------------------------------------------------------------------

instance ToJSON CAstCheck where
  toJSON c = object
    [ "checkName"   Aeson..=(c ^. #checkName)
    , "severity"    Aeson..=(c ^. #severity)
    , "description" Aeson..=(c ^. #description)
    , "explanation" Aeson..=(c ^. #explanation)
    , "remediation" Aeson..=(c ^. #remediation)
    , "cwe"         Aeson..=(c ^. #cwe)
    , "pattern"     Aeson..=(c ^. #pattern_)
    ]

instance FromJSON CAstCheck where
  parseJSON = withObject "CAstCheck" $ \o -> do
    checkName_   <- o .: "checkName"
    severity_    <- o .: "severity"
    description_ <- o .:? "description" >>= \case
      Just d  -> pure d
      Nothing -> pure checkName_
    explanation_ <- o .:? "explanation" >>= \case
      Just e  -> pure e
      Nothing -> pure "User-defined pattern."
    remediation_ <- o .:? "remediation" >>= \case
      Just r  -> pure r
      Nothing -> pure "Review the flagged code."
    cwe_         <- o .:? "cwe"
    pattern__    <- o .: "pattern" >>= stmtPatFromJSON
    pure CAstCheck
      { checkName   = checkName_
      , severity    = severity_
      , description = description_
      , explanation = explanation_
      , remediation = remediation_
      , cwe         = cwe_
      , pattern_    = pattern__
      }


-- ---------------------------------------------------------------------------
-- Store operations (IORef-based, same pattern as userPrims)
-- ---------------------------------------------------------------------------

-- | Get all user-defined checks from the IORef.
getUserChecks :: IORef [CAstCheck] -> IO [CAstCheck]
getUserChecks = readIORef

-- | Add a user-defined check. Replaces any existing check with the same name.
addUserCheck :: IORef [CAstCheck] -> CAstCheck -> IO ()
addUserCheck ref newCheck = modifyIORef' ref $ \checks ->
  newCheck : filter (\c -> c ^. #checkName /= newCheck ^. #checkName) checks

-- | Remove a user-defined check by name. Returns True if found.
removeUserCheck :: IORef [CAstCheck] -> Text -> IO Bool
removeUserCheck ref name = do
  before <- readIORef ref
  let after = filter (\c -> c ^. #checkName /= name) before
  writeIORef ref after
  pure $ length before /= length after

-- | List user-defined check names.
listUserChecks :: IORef [CAstCheck] -> IO [CAstCheck]
listUserChecks = readIORef

-- | Clear all user-defined checks.
clearUserChecks :: IORef [CAstCheck] -> IO ()
clearUserChecks ref = writeIORef ref []

-- | Get all checks (built-in + user-defined). User checks override built-in
--   checks with the same name.
getAllChecks :: IORef [CAstCheck] -> IO [CAstCheck]
getAllChecks ref = do
  user <- readIORef ref
  let userNames = HashSet.fromList $ fmap (^. #checkName) user
      builtins = filter (\c -> not $ HashSet.member (c ^. #checkName) userNames) allChecks
  pure $ user <> builtins

-- | Save user-defined checks to a JSON file.
saveUserChecks :: IORef [CAstCheck] -> FilePath -> IO (Either Text FilePath)
saveUserChecks ref path = do
  checks <- readIORef ref
  if null checks
    then pure $ Left "No user-defined patterns to save."
    else do
      LBS.writeFile path (AesonPretty.encodePretty checks)
      pure $ Right path

-- | Load checks from a JSON file and add them to the store.
--   Returns the names of loaded checks, or an error.
loadUserChecks :: IORef [CAstCheck] -> FilePath -> IO (Either Text [Text])
loadUserChecks ref path = do
  bs <- BS.readFile path
  case eitherDecodeStrict bs of
    Left err -> pure $ Left $ "JSON parse error: " <> cs err
    Right checks -> do
      forM_ (checks :: [CAstCheck]) $ \c -> addUserCheck ref c
      pure $ Right $ fmap (^. #checkName) checks
