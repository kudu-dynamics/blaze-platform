module Blaze.Import.Pil where

import Blaze.Prelude

import Blaze.Types.CallGraph (Function)
import Blaze.Types.Path.AlgaPath (AlgaPath)
import Blaze.Types.Pil (Stmt)

type Path = AlgaPath

class PilImporter a where
  getFuncStatements :: a -> Function -> IO [Stmt]
  getPathStatements :: a -> Path -> IO [Stmt]
