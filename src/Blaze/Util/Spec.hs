module Blaze.Util.Spec where

import Blaze.Prelude hiding (Symbol)

import qualified Blaze.Types.Cfg as Cfg
import Blaze.Function (Function (Function))
import Blaze.Pil.Construct qualified as C
import Blaze.Types.Pil (Ctx (Ctx), Expression, Stmt, Symbol, PilVar)
import qualified Blaze.Types.Pil as Pil
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

pilCall :: PilVar -> Function -> [Expression] -> Stmt
pilCall retVar func args =
  C.defCall' retVar (Pil.CallFunc func) args C.defaultSize

mkCallNode' :: Ctx -> Text -> PilVar -> Function -> [Expression] -> (Cfg.CallNode [Stmt], Stmt)
mkCallNode' ctx name retVar targetFunc' args =
  ( Cfg.CallNode
    { ctx = ctx
    , start = 0
    , callDest = Pil.CallFunc targetFunc'
    , uuid = uuid'
    , nodeData = [callStmt']
    }
  , callStmt'
  )
  where
    callStmt' = pilCall retVar targetFunc' args
    uuid' = mkUuid1 . hash $ (ctx ^. #func, name)

mkCallNode :: Ctx -> Text -> Symbol -> Function -> [Expression] -> (Cfg.CallNode [Stmt], Stmt)
mkCallNode ctx_ name retVarSym = mkCallNode' ctx_ name $ C.pilVar retVarSym
