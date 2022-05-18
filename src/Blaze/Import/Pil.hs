module Blaze.Import.Pil where

import Blaze.Prelude

import Blaze.Types.Function (Function)
import Blaze.Types.Pil (Stmt)
import Blaze.Types.Cfg (CodeReference)
import Blaze.Types.Pil.Common (CtxId)

class PilImporter a where
  type IndexType a
  getFuncStatements :: a -> Function -> CtxId -> IO [Stmt]
  getCodeRefStatements :: a -> CtxId -> CodeReference (IndexType a) -> IO [Stmt]
