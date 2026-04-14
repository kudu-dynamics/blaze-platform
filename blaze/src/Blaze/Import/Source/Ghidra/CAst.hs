-- | Convert Ghidra's parser-internal C-AST into the backend-agnostic
-- 'Blaze.Types.CAst' representation. The Ghidra AST carries JNI references
-- in its raw fallback variants; this module strips those by rendering the
-- unparsed subtrees to text. The result is fully serializable and contains
-- no Ghidra-specific state.
module Blaze.Import.Source.Ghidra.CAst
  ( toBlazeCStmts
  , toBlazeCStmt
  , toBlazeCExpr
  ) where

import Blaze.Prelude

import Blaze.Import.Source.Ghidra.Types (convertAddress)
import qualified Blaze.Types.CAst as B
import qualified Ghidra.Clang as G


toBlazeCStmts :: [G.CStmt] -> [B.CStmt]
toBlazeCStmts = fmap toBlazeCStmt

toBlazeCStmt :: G.CStmt -> B.CStmt
toBlazeCStmt = \case
  G.CExprStmt a e        -> B.CExprStmt (toAnn a) (toBlazeCExpr e)
  G.CIf a c body         -> B.CIf (toAnn a) (toBlazeCExpr c) (toBlazeCStmts body)
  G.CIfElse a c tb eb    -> B.CIfElse (toAnn a) (toBlazeCExpr c) (toBlazeCStmts tb) (toBlazeCStmts eb)
  G.CSwitch a c cases    -> B.CSwitch (toAnn a) (toBlazeCExpr c) (fmap toBlazeCCase cases)
  G.CWhile a c body      -> B.CWhile (toAnn a) (toBlazeCExpr c) (toBlazeCStmts body)
  G.CDoWhile a body c    -> B.CDoWhile (toAnn a) (toBlazeCStmts body) (toBlazeCExpr c)
  G.CFor a initC c i b   -> B.CFor (toAnn a) (toBlazeCForInit initC)
                                   (fmap toBlazeCExpr c) (fmap toBlazeCExpr i)
                                   (toBlazeCStmts b)
  G.CContinue a          -> B.CContinue (toAnn a)
  G.CBreak a             -> B.CBreak (toAnn a)
  G.CReturn a me         -> B.CReturn (toAnn a) (fmap toBlazeCExpr me)
  G.CVarDecl a ty nm me  -> B.CVarDecl (toAnn a) ty nm (fmap toBlazeCExpr me)
  G.CComment a t         -> B.CComment (toAnn a) t
  G.CBlock a body        -> B.CBlock (toAnn a) (toBlazeCStmts body)
  G.CRawStmt a nodes     -> B.CRawStmt (toAnn a) (mconcat (fmap G.clangText nodes))

toBlazeCExpr :: G.CExpr -> B.CExpr
toBlazeCExpr = \case
  G.CIdent     a t       -> B.CIdent     (toAnn a) t
  G.CLitInt    a i       -> B.CLitInt    (toAnn a) i
  G.CLitString a s       -> B.CLitString (toAnn a) s
  G.CBinaryOp  a op l r  -> B.CBinaryOp  (toAnn a) op (toBlazeCExpr l) (toBlazeCExpr r)
  G.CUnaryOp   a op e    -> B.CUnaryOp   (toAnn a) op (toBlazeCExpr e)
  G.CPostfixOp a op e    -> B.CPostfixOp (toAnn a) op (toBlazeCExpr e)
  G.CFuncall   a fn args -> B.CFuncall   (toAnn a) fn (fmap toBlazeCExpr args)
  G.CAssign    a op l r  -> B.CAssign    (toAnn a) op (toBlazeCExpr l) (toBlazeCExpr r)
  G.CIndex     a arr idx -> B.CIndex     (toAnn a) (toBlazeCExpr arr) (toBlazeCExpr idx)
  G.CDot       a base f  -> B.CDot       (toAnn a) (toBlazeCExpr base) f
  G.CArrow     a base f  -> B.CArrow     (toAnn a) (toBlazeCExpr base) f
  G.CCast      a ty e    -> B.CCast      (toAnn a) ty (toBlazeCExpr e)
  G.CCond      a c t e   -> B.CCond      (toAnn a) (toBlazeCExpr c) (toBlazeCExpr t) (toBlazeCExpr e)
  G.CRawExpr   a nodes   -> B.CRawExpr   (toAnn a) (mconcat (fmap G.clangText nodes))

toBlazeCCase :: G.CCase -> B.CCase
toBlazeCCase = \case
  G.CCase e body  -> B.CCase (toBlazeCExpr e) (toBlazeCStmts body)
  G.CDefault body -> B.CDefault (toBlazeCStmts body)

toBlazeCForInit :: G.CForInit -> B.CForInit
toBlazeCForInit = \case
  G.CForInitExpr me      -> B.CForInitExpr (fmap toBlazeCExpr me)
  G.CForInitDecl ty nm e -> B.CForInitDecl ty nm (fmap toBlazeCExpr e)

toAnn :: G.CAnn -> B.CAnn
toAnn = fmap toAddrRange

toAddrRange :: G.AddrRange -> B.AddrRange
toAddrRange r = B.AddrRange
  { B.minAddr = convertAddress (r ^. #minAddr)
  , B.maxAddr = convertAddress (r ^. #maxAddr)
  }
