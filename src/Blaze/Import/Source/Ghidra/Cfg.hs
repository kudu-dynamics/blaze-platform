module Blaze.Import.Source.Ghidra.Cfg where

import qualified Prelude as P

import Ghidra.Address (getAddressSpaceMap)
import qualified Ghidra.BasicBlock as BB
import Ghidra.State (GhidraState)
import qualified Ghidra.Core as Ghidra
import qualified Ghidra.Function as GFunc
import qualified Ghidra.Pcode as Pcode
import qualified Ghidra.PcodeBlock as PB
import qualified Ghidra.Types.Pcode as Pcode
import qualified Ghidra.State as GState
import Ghidra.Types.Pcode.Lifted (PcodeOp, Output(Output))
import qualified Ghidra.Types.Pcode.Lifted as P
import qualified Ghidra.Types.Address as GAddr
import qualified Ghidra.Types.Variable as GVar
import qualified Ghidra.Types.BasicBlock as GBB
import qualified Ghidra.Types.PcodeBlock as PB


import Ghidra.Types.Variable (HighVarNode, VarNode, VarType)
import qualified Ghidra.Types as J

import qualified Blaze.Graph as G
import Blaze.Import.Pil (PilImporter (IndexType, getCodeRefStatements))
import qualified Blaze.Import.Source.Ghidra.CallGraph as CGI
import Blaze.Import.Source.Ghidra.Types
import Blaze.Import.Source.Ghidra.Pil (IsVariable)
import Blaze.Prelude hiding (Symbol, succ, pred)
import Blaze.Types.Cfg (
  BasicBlockNode (BasicBlockNode),
  BranchType (
    FalseBranch,
    TrueBranch,
    UnconditionalBranch
  ),
  CallNode (CallNode),
  CfEdge (CfEdge),
  CfNode (
    BasicBlock,
    Call, EnterFunc, LeaveFunc, Grouping
  ),
  Cfg,
  MkCfgRootError(ZeroRootNodes, MultipleRootNodes),
  PilCfg,
  PilEdge,
  PilNode,
  getNodeData,
  mkCfg,
  mkCfgFindRoot,
 )
import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Cfg as Cfg
import Blaze.Types.Function (Function)
import Blaze.Types.Import (ImportResult (ImportResult))
import Blaze.Types.Pil (Stmt, CtxId, Ctx(Ctx))
import qualified Blaze.Types.Pil as Pil
import Control.Monad.Trans.Writer.Lazy (runWriterT, tell)
import Data.DList (DList)
import qualified Data.Map.Strict as Map
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HMap
import qualified Data.List.NonEmpty as NEList
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import Data.Tuple.Extra ((***))


data ConverterError
  = ExpectedConditionalOp Text
  deriving (Eq, Ord, Show, Generic, Hashable)

data ConverterState = ConverterState
  deriving (Show, Generic)

newtype Converter a = Converter { _runConverter :: ExceptT ConverterError (StateT ConverterState IO) a}
  deriving (Functor)
  deriving newtype (Applicative, Monad, MonadState ConverterState, MonadIO, MonadError ConverterError)

getRawPcodeForBasicBlock :: GhidraState -> GBB.BasicBlock -> IO [PcodeOp VarNode]
getRawPcodeForBasicBlock gs bb = do
  addrSpaceMap <- getAddressSpaceMap gs
  Pcode.getRawPcode gs addrSpaceMap $ bb ^. #handle

-- | Returns 'True' if the branch jumps to destBlockStart iff the branch cond is true.
-- Returns Nothing if the pcode op isn't a conditional branch.
isTrueCondBranch :: PcodeOp a -> GAddr.Address -> Maybe Bool
isTrueCondBranch pcodeOp destBlockStart = case pcodeOp of
  P.CBRANCH destInput _ -> case destInput ^. #value of
    P.Absolute addr -> Just $ addr == destBlockStart
    P.Relative off -> P.error $ "RelativeAddress not expected at end of basic block: offset: "
                                      <> show off
  _ -> Nothing

-- | Generates a single UUID for each basic block found in the graph.
-- TODO: make sure bbg ^. #nodes doesn't have duplicates
genBlockUUIDs :: GBB.BasicBlockGraph GBB.BasicBlock -> IO (Map GBB.BasicBlock UUID)
genBlockUUIDs bbg = fmap Map.fromList
  . traverse (\bb -> (bb,) <$> randomIO)
  $ bbg ^. #nodes

getEdgeSets :: forall a. Ord a  => GBB.BasicBlockGraph a -> Map a [a]
getEdgeSets bbg = foldr f Map.empty $ bbg ^. #edges
  where
    f :: (a, a)
      -> Map a [a]
      -> Map a [a]
    f (src, dst) = Map.alter g src
      where
        g :: Maybe [a] -> Maybe [a]
        g Nothing = Just [dst]
        g (Just xs) = Just $ dst:xs

mkCfNodeBasicBlock :: (GBB.BasicBlock -> IO [PcodeOp a]) -> Ctx -> GBB.BasicBlock -> IO (CfNode [PcodeOp a])
mkCfNodeBasicBlock getPcode ctx bb = do
  uuid <- randomIO
  pcode <- getPcode bb
  let startAddr = convertAddress $ bb ^. #startAddress
  endAddr <- convertAddress <$> BB.getMaxAddress bb
  return . BasicBlock $ BasicBlockNode ctx startAddr endAddr uuid pcode

getGhidraFunction :: GhidraState -> Function -> IO J.Function
getGhidraFunction gs fn = CGI.getFunction_ gs (fn ^. #address) >>= \case
    Nothing -> error $ "Couldn't find function at addr: " <> show (fn ^. #address)
    Just fn' -> return fn'

mkCfEdgesFromEdgeSet
  :: forall a. Show a
  => Function
  -> Map GBB.BasicBlock (CfNode [PcodeOp a])
  -> (GBB.BasicBlock, [GBB.BasicBlock])
  -> [CfEdge (CfNode [PcodeOp a])]
mkCfEdgesFromEdgeSet fn bbCfNodeMap (src, dests) = case dests of
  [dest] -> [CfEdge src' (bbCfNodeMap Map.! dest) Cfg.UnconditionalBranch]
  [a, b] -> case (checkTrue a, checkTrue b) of
    (False, True) -> [ CfEdge src' (bbCfNodeMap Map.! a) FalseBranch
                     , CfEdge src' (bbCfNodeMap Map.! b) TrueBranch
                     ]
    (True, False) -> [ CfEdge src' (bbCfNodeMap Map.! a) TrueBranch
                     , CfEdge src' (bbCfNodeMap Map.! b) FalseBranch
                     ]
    (False, False) -> error "Neither child is destination of CBRANCH"
    (True, True) -> error "Both children are destination of CBRANCH"
    where
      lastSrcStmt :: PcodeOp a
      lastSrcStmt = case lastMay (getNodeData src') of
        Nothing -> error "Block as two outgoing edges but contains no statements"
        Just stmt -> stmt

      -- | Returns True if start addr of basic block is CBranch dest
      checkTrue :: GBB.BasicBlock -> Bool
      checkTrue bb = case lastSrcStmt of
        P.CBRANCH inputDest _ -> case inputDest ^. #value of
          P.Absolute addr -> bb ^. #startAddress == addr
          P.Relative _ -> error $ "Unexpected relative address for CBRANCH"
        otherInstr -> do
          error $ "Unexpected instruction in " <> cs (fn ^. #name) <> ":\n" <> cs (pshow otherInstr)
  [] -> error "BasicBlock from EdgeSet has 0 outgoing edges" -- impossible
  xs -> error $ "Expected 1 or 2 edges, got " <> show (length xs)
  where
    src' = bbCfNodeMap Map.! src

type ThunkDestFunc = J.Function

getPcodeCfg
  :: (Show a, Hashable a)
  => (J.Function -> GhidraState -> GBB.BasicBlock -> IO [PcodeOp a])
  -> GhidraState
  -> CtxId
  -> Function
  -> IO (Either ThunkDestFunc (Cfg (CfNode [PcodeOp a])))
getPcodeCfg getPcode gs ctxId fn = do
  jfunc <- getGhidraFunction gs fn
  GFunc.isThunk jfunc >>= \case
    True -> do
      Left <$> GFunc.unsafeGetThunkedFunction jfunc
    False -> fmap Right $ do
      bbGraph <- BB.getBasicBlockGraph gs jfunc
      let ctx = Ctx fn ctxId
      bbCfNodeTuples <- traverse (\bb -> (bb,) <$> mkCfNodeBasicBlock (getPcode jfunc gs) ctx bb)
        $ bbGraph ^. #nodes
      let bbCfNodeMap = Map.fromList bbCfNodeTuples
          edgeSets = getEdgeSets bbGraph
          cfEdges = concatMap (mkCfEdgesFromEdgeSet fn bbCfNodeMap) . Map.toList $ edgeSets
      case mkCfgFindRoot ctxId (snd <$> bbCfNodeTuples) cfEdges of
        Left err -> case err of
          ZeroRootNodes -> do
            print $ length bbCfNodeTuples
            print bbGraph
            error "Cfg has no root node"
          MultipleRootNodes -> error "Cfg has more than one root node"
        Right cfg -> return cfg

getRawPcodeCfg
  :: GhidraState
  -> CtxId
  -> Function
  -> IO (Either ThunkDestFunc (Cfg (CfNode [PcodeOp VarNode])))
getRawPcodeCfg = getPcodeCfg $ const getRawPcodeForBasicBlock


------------- High Pcode Block -----------------

getHighPcodeForPcodeBlock
  :: GhidraState
  -> J.HighFunction
  -> PB.PcodeBlock
  -> IO [PcodeOp HighVarNode]
getHighPcodeForPcodeBlock gs hfunc pb = do
  addrSpaceMap <- getAddressSpaceMap gs
  Pcode.getHighPcode gs addrSpaceMap hfunc $ pb ^. #handle

mkCfNodePcodeBlock :: (PB.PcodeBlock -> IO [PcodeOp HighVarNode]) -> Ctx -> PB.PcodeBlock -> IO (CfNode [PcodeOp HighVarNode])
mkCfNodePcodeBlock getPcode ctx pb = do
  uuid <- randomIO
  pcode <- getPcode pb
  startAddr <- convertAddress <$> PB.getStart pb
  endAddr <- convertAddress <$> PB.getStop pb
  return . BasicBlock $ BasicBlockNode ctx startAddr endAddr uuid pcode

mkCfEdgeFromPcodeBlockEdge :: (PB.BranchType, (a, a)) -> CfEdge a
mkCfEdgeFromPcodeBlockEdge (bt, edges) = Cfg.fromTupleEdge $ case bt of
  PB.TrueBranch -> (Cfg.TrueBranch, edges)
  PB.FalseBranch -> (Cfg.FalseBranch, edges)
  _ -> (Cfg.UnconditionalBranch, edges)

getHighPcodeCfg
  :: GhidraState
  -> CtxId
  -> Function
  -> IO (Either ThunkDestFunc (Cfg (CfNode [PcodeOp HighVarNode])))
getHighPcodeCfg gs ctxId fn = do
  jfunc <- getGhidraFunction gs fn
  GFunc.isThunk jfunc >>= \case
    True -> do
      Left <$> GFunc.unsafeGetThunkedFunction jfunc
    False -> fmap Right $ do
      hfunc <- GFunc.getHighFunction gs jfunc -- TODO: cache this
      bbGraph <- PB.getPcodeBlockGraph hfunc
      let ctx = Ctx fn ctxId
      nodePcodeTuples <- traverse (\pb -> (pb,) <$> mkCfNodePcodeBlock (getHighPcodeForPcodeBlock gs hfunc) ctx pb)
        $ bbGraph ^. #nodes
      let nodeMap = Map.fromList nodePcodeTuples
          bbGraph' = (nodeMap Map.!) <$> bbGraph
          edges' = mkCfEdgeFromPcodeBlockEdge <$> (bbGraph' ^. #edges)
          nodes' = snd <$> nodePcodeTuples
      case mkCfgFindRoot ctxId nodes' edges' of
        Left err -> case err of
          ZeroRootNodes -> do
            print $ length nodePcodeTuples
            print bbGraph
            error "Cfg has no root node"
          MultipleRootNodes -> error "Cfg has more than one root node"
        Right cfg -> return cfg

