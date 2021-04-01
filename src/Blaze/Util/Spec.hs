module Blaze.Util.Spec where

import qualified Blaze.Cfg as Cfg
import Blaze.Cfg (CfNode, CfEdge, BranchType)
import Blaze.Function (Function)
import Blaze.Prelude
import qualified Blaze.Types.Graph.Unique as U
import Blaze.Types.Graph.Unique (Unique)
import qualified Data.UUID as UUID
import qualified Blaze.Graph as G


---------- CFG ------------

-- mkCfEdge :: Unique (CfNode a) -> Unique (CfNode a) -> BranchType -> CfEdgeUnique a
-- mkCfEdge a b bt = G.LEdge bt (G.Edge a b)

-- mkUnique2 :: Word64 -> Word64 -> a -> Unique a
-- mkUnique2 a b = U.Unique . U.NodeId $ UUID.fromWords64 a b

-- bb :: Function -> Address -> Address -> a -> Unique (CfNode a)
-- bb func startAddr endAddr x = mkUnique2
--   (fromIntegral startAddr)
--   (fromIntegral endAddr)
--   . Cfg.BasicBlock
--   $ Cfg.BasicBlockNode func startAddr endAddr x

-- mkEdge :: (a, a) -> G.Edge a
-- mkEdge (a, b) = G.Edge a b

-- mkLEdge :: (e, (a, a)) -> G.LEdge e a
-- mkLEdge (lbl, e) = G.LEdge lbl $ mkEdge e
