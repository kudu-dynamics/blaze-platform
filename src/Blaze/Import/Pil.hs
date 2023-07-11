module Blaze.Import.Pil where

import Blaze.Prelude

import Blaze.Types.Function (Function)
import Blaze.Types.Pil (Stmt, MappedStmt)
import Blaze.Types.Cfg (CodeReference)
import Blaze.Types.Pil.Common (CtxId)

class PilImporter a where
  type IndexType a
  getFuncStatements :: a -> Function -> CtxId -> IO [Stmt]
  getMappedStatements :: a -> Function -> CtxId -> IO [MappedStmt Address]
  getCodeRefStatements :: a -> CtxId -> CodeReference (IndexType a) -> IO [Stmt]
