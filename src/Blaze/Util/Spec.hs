module Blaze.Util.Spec where

import qualified Blaze.Types.Cfg as Cfg
import Blaze.Prelude
import Blaze.Function (Function (Function))
import Blaze.Types.Pil (Ctx (Ctx))
import qualified Data.UUID as UUID

---------- CFG ------------

mkUuid1 :: Integral a => a -> UUID
mkUuid1 = UUID.fromWords64 0 . fromIntegral

mkUuid2 :: Integral a => a -> a -> UUID
mkUuid2 a b = UUID.fromWords64 (fromIntegral a) (fromIntegral b)

bb :: Ctx -> Address -> Address -> a -> Cfg.CfNode a
bb ctx startAddr endAddr x =
  Cfg.BasicBlock $ Cfg.BasicBlockNode ctx startAddr endAddr uuid x
 where
  uuid = mkUuid2 startAddr endAddr

mkDummyCtx :: Word64 -> Ctx
mkDummyCtx = Ctx (Function Nothing "dummyCtx" 0x00 []) . fromIntegral

mkDummyTermNode :: Ctx -> a -> Cfg.CfNode a
mkDummyTermNode ctx nodeData 
  = Cfg.BasicBlock
    $ Cfg.BasicBlockNode
    { ctx = ctx
    , start = 0
    , end = 0
    , uuid = mkUuid1 (0 :: Int)
    , nodeData = nodeData
    }
