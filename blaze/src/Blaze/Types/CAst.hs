-- | Backend-agnostic representation of a decompiled C AST.
--
-- This is the canonical type that Blaze.Import.Decomp returns. It deliberately
-- carries no importer-specific state (no JNI handles, no Ghidra opaque trees),
-- so it is fully serializable and safe to cache, persist, and pass between
-- backends. Importers convert their internal representation into this shape;
-- consumers (matchers, persisters, renderers) only ever see this type.
module Blaze.Types.CAst
  ( -- * Annotations
    AddrRange (..)
  , CAnn

    -- * Statements
  , CStmt (..)
  , CForInit (..)
  , CCase (..)
  , stmtAnn

    -- * Expressions
  , CExpr (..)
  , exprAnn

    -- * Rendering
  , renderStmts
  , renderStmt
  , renderExpr
  , renderForInit
  , renderCase
  ) where

import Blaze.Prelude

import qualified Data.Text as T


-- | A binary-address range tied back to a C-AST node. The matcher reports
-- these in 'CAstFinding' so users can navigate from a finding to the source
-- bytes; persistence and equality are content-based.
data AddrRange = AddrRange
  { minAddr :: Address
  , maxAddr :: Address
  } deriving (Eq, Ord, Show, Generic, Hashable)
    deriving anyclass (Serialize)

-- | A C AST node may collect multiple address ranges from its children. The
-- list preserves child order and avoids merging — finer-grained than a single
-- range — so callers can attribute each component to its source.
type CAnn = [AddrRange]


-- ---------------------------------------------------------------------------
-- Statements
-- ---------------------------------------------------------------------------

-- | Structured C statements. 'CRawStmt' is the catch-all variant for source
-- the importer could not parse into a structured form — it carries the
-- pretty-printed text of the unparsed subtree so future matchers and human
-- readers can still inspect it, with no backend-specific payload.
data CStmt
  = CExprStmt CAnn CExpr
  | CIf       CAnn CExpr [CStmt]
  | CIfElse   CAnn CExpr [CStmt] [CStmt]
  | CSwitch   CAnn CExpr [CCase]
  | CWhile    CAnn CExpr [CStmt]
  | CDoWhile  CAnn [CStmt] CExpr
  | CFor      CAnn CForInit (Maybe CExpr) (Maybe CExpr) [CStmt]
    -- ^ ann, init, cond, incr, body
  | CContinue CAnn
  | CBreak    CAnn
  | CReturn   CAnn (Maybe CExpr)
  | CVarDecl  CAnn Text Text (Maybe CExpr)
    -- ^ ann, type, name, init
  | CComment  CAnn Text
  | CBlock    CAnn [CStmt]
  | CRawStmt  CAnn Text
    -- ^ catch-all; payload is the rendered source of the unparsed subtree
  deriving (Eq, Ord, Show, Generic, Hashable)
  deriving anyclass (Serialize)

-- | For-loop init clause — declaration form, expression form, or empty.
data CForInit
  = CForInitExpr (Maybe CExpr)            -- ^ for(expr; ...) or for(; ...)
  | CForInitDecl Text Text (Maybe CExpr)  -- ^ for(type name = expr; ...)
  deriving (Eq, Ord, Show, Generic, Hashable)
  deriving anyclass (Serialize)

data CCase
  = CCase CExpr [CStmt]
  | CDefault [CStmt]
  deriving (Eq, Ord, Show, Generic, Hashable)
  deriving anyclass (Serialize)


-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

-- | Structured C expressions. 'CRawExpr', like 'CRawStmt', preserves the
-- source text of any subtree the importer couldn't parse.
data CExpr
  = CIdent     CAnn Text
  | CLitInt    CAnn Integer
  | CLitString CAnn Text
  | CBinaryOp  CAnn Text CExpr CExpr
  | CUnaryOp   CAnn Text CExpr
  | CPostfixOp CAnn Text CExpr
  | CFuncall   CAnn Text [CExpr]
  | CAssign    CAnn Text CExpr CExpr
    -- ^ ann, op, lhs, rhs
  | CIndex     CAnn CExpr CExpr
  | CDot       CAnn CExpr Text
  | CArrow     CAnn CExpr Text
  | CCast      CAnn Text CExpr
  | CCond      CAnn CExpr CExpr CExpr
  | CRawExpr   CAnn Text
    -- ^ catch-all; payload is the rendered source of the unparsed subtree
  deriving (Eq, Ord, Show, Generic, Hashable)
  deriving anyclass (Serialize)


-- ---------------------------------------------------------------------------
-- Annotation accessors
-- ---------------------------------------------------------------------------

exprAnn :: CExpr -> CAnn
exprAnn = \case
  CIdent     a _     -> a
  CLitInt    a _     -> a
  CLitString a _     -> a
  CBinaryOp  a _ _ _ -> a
  CUnaryOp   a _ _   -> a
  CPostfixOp a _ _   -> a
  CFuncall   a _ _   -> a
  CAssign    a _ _ _ -> a
  CIndex     a _ _   -> a
  CDot       a _ _   -> a
  CArrow     a _ _   -> a
  CCast      a _ _   -> a
  CCond      a _ _ _ -> a
  CRawExpr   a _     -> a

stmtAnn :: CStmt -> CAnn
stmtAnn = \case
  CExprStmt a _       -> a
  CIf       a _ _     -> a
  CIfElse   a _ _ _   -> a
  CSwitch   a _ _     -> a
  CWhile    a _ _     -> a
  CDoWhile  a _ _     -> a
  CFor      a _ _ _ _ -> a
  CContinue a         -> a
  CBreak    a         -> a
  CReturn   a _       -> a
  CVarDecl  a _ _ _   -> a
  CComment  a _       -> a
  CBlock    a _       -> a
  CRawStmt  a _       -> a


-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

renderStmts :: Int -> [CStmt] -> Text
renderStmts indent = mconcat . fmap (renderStmt indent)

renderStmt :: Int -> CStmt -> Text
renderStmt n stmt =
  let pad = T.replicate (n * 2) " "
  in case stmt of
    CExprStmt _ expr -> pad <> renderExpr expr <> ";\n"
    CIf _ cond body ->
      pad <> "if (" <> renderExpr cond <> ") {\n"
      <> renderStmts (n + 1) body
      <> pad <> "}\n"
    CIfElse _ cond thenB elseB ->
      pad <> "if (" <> renderExpr cond <> ") {\n"
      <> renderStmts (n + 1) thenB
      <> pad <> "} else {\n"
      <> renderStmts (n + 1) elseB
      <> pad <> "}\n"
    CSwitch _ cond cases ->
      pad <> "switch (" <> renderExpr cond <> ") {\n"
      <> mconcat (fmap (renderCase (n + 1)) cases)
      <> pad <> "}\n"
    CWhile _ cond body ->
      pad <> "while (" <> renderExpr cond <> ") {\n"
      <> renderStmts (n + 1) body
      <> pad <> "}\n"
    CDoWhile _ body cond ->
      pad <> "do {\n"
      <> renderStmts (n + 1) body
      <> pad <> "} while (" <> renderExpr cond <> ");\n"
    CFor _ initC cond incr body ->
      pad <> "for (" <> renderForInit initC <> "; "
      <> maybe "" renderExpr cond <> "; "
      <> maybe "" renderExpr incr <> ") {\n"
      <> renderStmts (n + 1) body
      <> pad <> "}\n"
    CContinue _ -> pad <> "continue;\n"
    CBreak _    -> pad <> "break;\n"
    CReturn _ Nothing      -> pad <> "return;\n"
    CReturn _ (Just expr)  -> pad <> "return " <> renderExpr expr <> ";\n"
    CVarDecl _ typeName varName initExpr ->
      pad <> typeName <> " " <> varName
      <> maybe "" (\e -> " = " <> renderExpr e) initExpr <> ";\n"
    CComment _ txt -> pad <> "/* " <> T.strip txt <> " */\n"
    CBlock _ stmts ->
      pad <> "{\n" <> renderStmts (n + 1) stmts <> pad <> "}\n"
    CRawStmt _ raw ->
      pad <> "/* raw: " <> raw <> " */\n"

renderForInit :: CForInit -> Text
renderForInit (CForInitExpr Nothing)        = ""
renderForInit (CForInitExpr (Just e))       = renderExpr e
renderForInit (CForInitDecl t v Nothing)    = t <> " " <> v
renderForInit (CForInitDecl t v (Just e))   = t <> " " <> v <> " = " <> renderExpr e

renderCase :: Int -> CCase -> Text
renderCase n (CCase expr stmts) =
  let pad = T.replicate (n * 2) " "
  in pad <> "case " <> renderExpr expr <> ":\n" <> renderStmts (n + 1) stmts
renderCase n (CDefault stmts) =
  let pad = T.replicate (n * 2) " "
  in pad <> "default:\n" <> renderStmts (n + 1) stmts

renderExpr :: CExpr -> Text
renderExpr = \case
  CIdent     _ name      -> name
  CLitInt    _ i         -> show i
  CLitString _ s         -> "\"" <> s <> "\""
  CBinaryOp  _ op l r    -> renderExpr l <> " " <> op <> " " <> renderExpr r
  CUnaryOp   _ op e      -> op <> renderExpr e
  CPostfixOp _ op e      -> renderExpr e <> op
  CFuncall   _ name args -> name <> "(" <> T.intercalate ", " (fmap renderExpr args) <> ")"
  CAssign    _ op l r    -> renderExpr l <> " " <> op <> " " <> renderExpr r
  CIndex     _ arr idx   -> renderExpr arr <> "[" <> renderExpr idx <> "]"
  CDot       _ expr fld  -> renderExpr expr <> "." <> fld
  CArrow     _ expr fld  -> renderExpr expr <> "->" <> fld
  CCast      _ tyName e  -> "(" <> tyName <> ")" <> renderExpr e
  CCond      _ c t e     -> renderExpr c <> " ? " <> renderExpr t <> " : " <> renderExpr e
  CRawExpr   _ raw       -> "/* raw: " <> raw <> " */"
