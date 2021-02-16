module Blaze.Cfg.Interprocedural (
  module Blaze.Cfg.Interprocedural,
  module Exports,
) where

import Blaze.Prelude

import Blaze.Cfg
import Blaze.Types.Cfg (Cfg (Cfg), PilCallNode, PilNode)
import Blaze.Types.Cfg.Interprocedural as Exports
import Blaze.Types.Pil (Stmt)

getCallTarget :: PilCallNode -> Maybe Function
getCallTarget node =
  undefined

{- | Expand a call by substituting a call node with the CFG corresponding to the
 call destination.
-}
expandCall :: InterCfg -> PilCallNode -> Builder a (Maybe InterCfg)
expandCall icfg callNode = do
  case (use getCfg) targetFunc of
    Just targetCfg -> 
      return $ Just (substNode icfg callNode (InterCfg targetCfg))

{- | Substitute a node with another interprocedural CFG.
-}
substNode :: InterCfg -> PilNode -> InterCfg -> InterCfg
substNode
  (InterCfg (Cfg origGraph origRoot))
  node
  (InterCfg (Cfg replGraph replRoot)) =
    if origRoot /= node
      then InterCfg (Cfg graph origRoot)
      else
        let newRoot :: PilNode
            newRoot = replRoot
         in InterCfg (Cfg graph newRoot)
   where
    graph :: ControlFlowGraph [Stmt]
    graph = undefined