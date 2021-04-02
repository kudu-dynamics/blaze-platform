module Blaze.Util.Spec where

import qualified Blaze.Cfg as Cfg
import Blaze.Function (Function)
import Blaze.Prelude
import qualified Data.UUID as UUID

---------- CFG ------------

mkUuid1 :: Integral a => a -> UUID
mkUuid1 = UUID.fromWords64 0 . fromIntegral

mkUuid2 :: Integral a => a -> a -> UUID
mkUuid2 a b = UUID.fromWords64 (fromIntegral a) (fromIntegral b)

bb :: Function -> Address -> Address -> a -> Cfg.CfNode a
bb ctx startAddr endAddr x = 
  Cfg.BasicBlock $ Cfg.BasicBlockNode ctx startAddr endAddr uuid x
  where
    uuid = mkUuid2 startAddr endAddr
