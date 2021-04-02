module Blaze.Util.Spec where

import qualified Blaze.Cfg as Cfg
import Blaze.Function (Function)
import Blaze.Prelude


---------- CFG ------------

bb :: Function -> Address -> Address -> a -> Cfg.CfNode a
bb func startAddr endAddr x = 
  Cfg.BasicBlock $ Cfg.BasicBlockNode func startAddr endAddr Nothing x
