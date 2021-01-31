module Blaze.Import.Pil where

import Blaze.Prelude

import Blaze.Types.CallGraph (Function)
import Blaze.Types.Cfg (NodeRefMapEntry)
import Blaze.Types.Path.AlgaPath (AlgaPath)
import Blaze.Types.Pil (Stmt)

type Path = AlgaPath

class PilImporter a b c | a -> b c where
  getFuncStatements :: a -> Function -> IO [Stmt]
  getPathStatements :: a -> Path -> IO [Stmt]
  getNodeStatements :: a -> NodeRefMapEntry b c -> IO [Stmt]
