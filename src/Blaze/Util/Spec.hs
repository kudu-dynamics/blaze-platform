module Blaze.Util.Spec where

import qualified Blaze.Cfg as Cfg
import Blaze.Function (Function)
import Blaze.Prelude
import qualified Data.UUID as UUID

---------- CFG ------------

mkUuid2 :: Integral a => a -> a -> UUID
mkUuid2 a b = UUID.fromWords64 (fromIntegral a) (fromIntegral b)

bb :: Function -> Address -> Address -> a -> Cfg.CfNode a
bb func startAddr endAddr x = 
  Cfg.BasicBlock $ Cfg.BasicBlockNode func startAddr endAddr uuid x
  where
    uuid = mkUuid2 startAddr endAddr
