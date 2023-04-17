{-| Rewrite rules for Pil exprs and statements --}

module Blaze.Pil.Analysis.Rewrite
  ( module Blaze.Pil.Analysis.Rewrite
  ) where

import Blaze.Prelude

import Blaze.Types.Pil (Stmt)
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Pil.Construct as C


rewriteStmt :: Stmt -> [Stmt]
rewriteStmt stmt = case stmt of
  Pil.Call callOp -> case callOp ^. #name of
    Just "CopyMem" -> case callOp ^. #args of
      [dest, src, Pil.Expression _ (Pil.CONST x)] ->
        [ C.store dest . C.load src . fromIntegral $ x ^. #constant ]
      _ -> [stmt]
    _ -> [stmt]
  _ -> [stmt]

rewriteStmts :: [Stmt] -> [Stmt]
rewriteStmts = concatMap rewriteStmt
