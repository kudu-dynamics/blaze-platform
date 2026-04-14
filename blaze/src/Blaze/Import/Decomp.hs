-- | Decompiler importer interface.
--
-- Backends that can produce a decompiled view of a function — text or
-- structured C AST — provide a 'DecompImporter' instance. Kept separate from
-- 'Blaze.Import.Binary' so backends without a decompiler don't have to fake
-- one, and so consumers that only care about decompilation don't pull in
-- binary-loading concerns.
module Blaze.Import.Decomp
  ( DecompImporter (..)
  ) where

import Blaze.Prelude
import Blaze.Types.CAst (CStmt)


class DecompImporter a where
  -- | Decompile the function at the given address to C source text.
  -- Returns 'Nothing' if no function exists at that address.
  decompileFunctionText :: a -> Address -> IO (Maybe Text)
  decompileFunctionText _ _ = return Nothing

  -- | Decompile the function at the given address to a structured C AST.
  -- Returns 'Nothing' if no function exists at that address. The result is
  -- the backend-agnostic 'Blaze.Types.CAst.CStmt' — backends convert from
  -- their internal representation before returning.
  decompileFunctionAst :: a -> Address -> IO (Maybe [CStmt])
  decompileFunctionAst _ _ = return Nothing
