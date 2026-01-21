module Blaze.Import.Pil where

import Blaze.Prelude

import Blaze.Types.Function (Function)
import Blaze.Types.Pil (Stmt, MappedStmt)
import Blaze.Types.Cfg (CodeReference)
import Blaze.Types.Pil.Common (CtxId)
import Blaze.Types.Import (TypeHints)

class PilImporter a where
  type IndexType a
  getFuncStatements :: a -> Function -> CtxId -> IO [Stmt]
  getMappedStatements :: a -> Function -> CtxId -> IO [MappedStmt Address]
  getCodeRefStatements :: a -> CtxId -> CodeReference (IndexType a) -> IO [Stmt]
  getFuncStatementsWithTypeHints :: a -> Function -> CtxId -> IO ([Stmt], TypeHints)
  getMappedStatementsWithTypeHints :: a -> Function -> CtxId -> IO ([MappedStmt Address], TypeHints)
