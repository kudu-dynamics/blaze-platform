{-# LANGUAGE TemplateHaskell #-}
module Haze.Pil.Path where

import Haze.Prelude
import Haze.Types.Path (Path, Node(SubBlock), SubBlockNode)
import Hinja.Function (Function)
import qualified Haze.Types.Path as Path
import Haze.Types.Pil ( Statement
                      , Expression
                      , Ctx(Ctx)
                      , Stmt
                      )
import qualified Haze.Types.Pil as Pil
import Haze.Types.Pil (Converter, runConverter)
import qualified Data.Map as Map
import qualified Data.Set as Set
-- convert path to [Pil]


updateCtx :: Function -> Converter ()
updateCtx fn = do
  Pil.func .= Just fn
  Pil.ctxIndex %= g
  where
    g Nothing = Just 1
    g (Just n) = Just (n + 1)

convertSubBlockNode :: SubBlockNode -> Converter [Stmt]
convertSubBlockNode sb = do
  updateCtx $ sb ^. Path.func
  
  return []

convertNode :: Node -> Converter [Stmt]
convertNode (SubBlock x) = convertSubBlockNode x
convertNode _ = return [] -- TODO

startCtx :: Ctx
startCtx = Ctx Nothing Nothing Set.empty Map.empty

convertPath :: Path p => p -> IO [Stmt]
convertPath =
  fmap (concat . fst) . flip runConverter startCtx . traverse convertNode . Path.toList


