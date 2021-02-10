module Blaze.Import.Pil where

import Blaze.Prelude

import Blaze.Types.CallGraph (Function)
import Blaze.Types.Path.AlgaPath (AlgaPath)
import Blaze.Types.Pil (Stmt)
import Blaze.Types.Cfg (CodeReference)

type Path = AlgaPath

class PilImporter a b | a -> b where
  getFuncStatements :: a -> Function -> IO [Stmt]
  getPathStatements :: a -> Path -> IO [Stmt]
  getCodeRefStatements :: a -> CodeReference b -> IO [Stmt]
