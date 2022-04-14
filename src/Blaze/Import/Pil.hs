module Blaze.Import.Pil where

import Blaze.Prelude

import Blaze.Types.Function (Function)
import Blaze.Types.Path.AlgaPath (AlgaPath)
import Blaze.Types.Pil (Stmt)
import Blaze.Types.Cfg (CodeReference)
import Blaze.Types.Pil.Common (CtxId)

type Path = AlgaPath

class PilImporter a where
  type IndexType a
  getFuncStatements :: a -> Function -> CtxId -> IO [Stmt]
  getPathStatements :: a -> Path -> IO [Stmt]
  getCodeRefStatements :: a -> CtxId -> CodeReference (IndexType a) -> IO [Stmt]
