module Blaze.Import.Pil where

import Blaze.Types.CallGraph (Function)
import Blaze.Types.Path.AlgaPath (AlgaPath)
import Blaze.Types.Pil (Stmt)

type Path = AlgaPath

class PilImporter a where
  getFuncStatements :: a -> Function -> [Stmt]
  getPathStatements :: a -> Path -> [Stmt]
