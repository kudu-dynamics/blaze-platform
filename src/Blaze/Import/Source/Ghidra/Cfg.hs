module Blaze.Import.Source.Ghidra.Cfg where

import qualified Prelude as P

import Ghidra.Address (getAddressSpaceMap)
import qualified Ghidra.BasicBlock as BB
import Ghidra.State (GhidraState)
import qualified Ghidra.Core as Ghidra
import qualified Ghidra.Function as GFunc
import qualified Ghidra.Pcode as Pcode
import qualified Ghidra.Types.Pcode as Pcode
import qualified Ghidra.State as GState
import Ghidra.Types.Pcode.Lifted (PcodeOp, Output(Output))
import qualified Ghidra.Types.Pcode.Lifted as P
import qualified Ghidra.Types.Address as GAddr
import qualified Ghidra.Types.Variable as GVar
import qualified Ghidra.Types.BasicBlock as GBB
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
  PilCfg,
  PilEdge,
  PilNode,
  getNodeData,
  mkCfg,
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

getHighPcodeForBasicBlock :: GhidraState -> J.HighFunction -> GBB.BasicBlock -> IO [PcodeOp HighVarNode]
getHighPcodeForBasicBlock gs hfunc bb = do
  addrSpaceMap <- getAddressSpaceMap gs
  Pcode.getHighPcode gs addrSpaceMap hfunc $ bb ^. #handle

-- | Returns 'True' if the branch jumps to destBlockStart iff the branch cond is true.
-- Returns Nothing if the pcode op isn't a conditional branch.
isTrueCondBranch :: PcodeOp a -> GAddr.Address -> Maybe Bool
isTrueCondBranch pcodeOp destBlockStart = case pcodeOp of
  P.CBRANCH destInput _ -> case destInput ^. #value of
    P.Absolute addr -> Just $ addr == destBlockStart
    P.Relative off -> P.error $ "RelativeAddress not expected at end of basic block: offset: "
                                      <> show off
  _ -> Nothing
  

data OutgoingBranchResults a = OutgoingBranchResults
  { block :: GBB.BasicBlock
  , blockData :: PcodeOp a
  , branchType :: BranchType
  }
  deriving (Eq, Ord, Show, Generic)

-- makeOutgoingBranches :: (GBB.BasicBlock -> IO (NonEmpty a)) -> GBB.BasicBlock -> [GBB.BasicBlock] -> IO (OutgoingBranchResults [a])
-- makeOutgoingBranches = undefined

-- get basic block graph
-- get [PcodeOp a] for each BB node, make map of BB -> [PcodeOp a]
-- get branchtypes for each (BB, [PcodeOp a]) from mapping
--   + any with 2 branches get conditionals
--   + any with 1 get fallthrough
--   + error if more than one

-- | Generates a single UUID for each basic block found in the graph.
-- TODO: make sure bbg ^. #nodes doesn't have duplicates
genBlockUUIDs :: GBB.BasicBlockGraph GBB.BasicBlock -> IO (Map GBB.BasicBlock UUID)
genBlockUUIDs bbg = fmap Map.fromList
  . traverse (\bb -> (bb,) <$> randomIO)
  $ bbg ^. #nodes

getEdgeSets :: GBB.BasicBlockGraph GBB.BasicBlock -> Map GBB.BasicBlock [GBB.BasicBlock]
getEdgeSets bbg = foldr f Map.empty $ bbg ^. #edges
  where
    f :: (GBB.BasicBlock, GBB.BasicBlock)
      -> Map GBB.BasicBlock [GBB.BasicBlock]
      -> Map GBB.BasicBlock [GBB.BasicBlock]
    f (src, dst) = Map.alter g src
      where
        g :: Maybe [GBB.BasicBlock] -> Maybe [GBB.BasicBlock]
        g Nothing = Just [dst]
        g (Just xs) = Just $ dst:xs

mkCfNodeBasicBlock :: GhidraState -> Ctx -> GBB.BasicBlock -> IO (CfNode [PcodeOp VarNode])
mkCfNodeBasicBlock gs ctx bb = do
  uuid <- randomIO
  pcode <- getRawPcodeForBasicBlock gs bb
  let startAddr = convertAddress $ bb ^. #startAddress
  endAddr <- convertAddress <$> BB.getMaxAddress bb
  return . BasicBlock $ BasicBlockNode ctx startAddr endAddr uuid pcode

getGhidraFunction :: GhidraState -> Function -> IO J.Function
getGhidraFunction gs fn = CGI.getFunction_ gs (fn ^. #address) >>= \case
    Nothing -> error $ "Couldn't find function at addr: " <> show (fn ^. #address)
    Just fn' -> return fn'

mkCfEdgesFromEdgeSet :: Map GBB.BasicBlock (CfNode [PcodeOp VarNode]) -> (GBB.BasicBlock, [GBB.BasicBlock]) -> [CfEdge (CfNode [PcodeOp VarNode])]
mkCfEdgesFromEdgeSet bbCfNodeMap (src, dests) = case dests of
  [dest] -> [CfEdge src' (bbCfNodeMap Map.! dest) Cfg.UnconditionalBranch]
  [a, b] -> case sort [checkTrue a, checkTrue b] of
    [False, True] -> [ CfEdge src' (bbCfNodeMap Map.! a) FalseBranch
                     , CfEdge src' (bbCfNodeMap Map.! b) TrueBranch
                     ]
    [True, False] -> [ CfEdge src' (bbCfNodeMap Map.! a) TrueBranch
                     , CfEdge src' (bbCfNodeMap Map.! b) FalseBranch
                     ]
    [False, False] -> error "Neither child is destination of CBRANCH"
    [True, True] -> error "Both children are destination of CBRANCH"
    where
      lastSrcStmt :: PcodeOp VarNode
      lastSrcStmt = case lastMay (getNodeData src') of
        Nothing -> error "Block as two outgoing edges but contains no statements"
        Just stmt -> stmt

      -- | Returns True if start addr of basic block is CBranch dest
      checkTrue :: GBB.BasicBlock -> Bool
      checkTrue bb = case lastSrcStmt of
        P.CBRANCH inputDest _ -> case inputDest ^. #value of
          P.Absolute addr -> bb ^. #startAddress == addr
          P.Relative _ -> error $ "Unexpected relative address for CBRANCH"
        otherInstr -> error $ "Unexpected instruction: " <> show otherInstr
  [] -> error "BasicBlock from EdgeSet has 0 outgoing edges" -- impossible
  xs -> error $ "Expected 1 or 2 edges, got " <> show (length xs)
  where
    src' = bbCfNodeMap Map.! src
    dests' = (bbCfNodeMap Map.!) <$> dests

getRawPcodeCfg :: GhidraState -> Function -> CtxId -> IO (Cfg (CfNode [PcodeOp VarNode]))
getRawPcodeCfg gs fn ctxId = do
  jfunc <- getGhidraFunction gs fn
  bbGraph <- BB.getBasicBlockGraph gs jfunc
  let ctx = Ctx fn ctxId
  -- uuidMap <- genBlockUUIDs bbGraph
  -- bbPcodeMap <- traverse (\bb -> (bb,) <$> getRawPcodeForBasicBlock gs bb)
  --   $ bbGraph ^. #nodes
  bbCfNodeMap <- fmap Map.fromList
    . traverse (\bb -> (bb,) <$> mkCfNodeBasicBlock gs ctx bb)
    $ bbGraph ^. #nodes
  let edgeSets = getEdgeSets bbGraph
  let cfEdges = concatMap (mkCfEdgesFromEdgeSet bbCfNodeMap) . Map.toList $ edgeSets
  undefined
  

-- getRawPcodeCfg :: GhidraState -> GFunc.Function -> IO [PcodeOp HighVarNode]



-- toMlilSsaInstr :: MlilSsaInstruction -> MlilSsaInstr
-- toMlilSsaInstr instr' =
--   maybe (MlilSsaNonCall $ NonCallInstruction instr') MlilSsaCall (toCallInstruction instr')

-- runNodeConverter :: NodeConverter a -> IO (a, DList MlilNodeRefMapEntry)
-- runNodeConverter = runWriterT

-- tellEntry :: MlilNodeRefMapEntry -> NodeConverter ()
-- tellEntry = tell . DList.singleton

-- -- | Assumes instructions are consecutive
-- nodeFromInstrs :: Ctx -> NonEmpty NonCallInstruction -> NodeConverter MlilSsaCfNode
-- nodeFromInstrs ctx instrs = do
--   uuid' <- liftIO randomIO
--   let node =
--         BasicBlock $
--           BasicBlockNode
--             { ctx = ctx
--             , start = (view Mlil.address . unNonCallInstruction) . NEList.head $ instrs
--             , end = (view Mlil.address . unNonCallInstruction) . NEList.last $ instrs
--             , nodeData = unNonCallInstruction <$> instrs
--             , uuid = uuid'
--             }
--   tellEntry
--     ( node
--     , Cfg.CodeReference
--         { function = ctx ^. #func
--         , startIndex = view Mlil.index . unNonCallInstruction . NEList.head $ instrs
--         , endIndex = view Mlil.index . unNonCallInstruction . NEList.last $ instrs
--         }
--     )
--   return node

-- -- TODO: Here we are using a NonEmpty but current refactor is asserting that nodeData is a "[a]"
-- nodeFromCallInstr :: Ctx -> CallInstruction -> NodeConverter MlilSsaCfNode
-- nodeFromCallInstr ctx callInstr' = do
--   uuid' <- liftIO randomIO
--   let node =
--         Call $
--           CallNode
--             { ctx = ctx
--             , start = callInstr' ^. #address
--             , callDest = Pil.CallUnk
--             , nodeData = callInstr' ^. #instr :| []
--             , uuid = uuid'
--             }
--   tellEntry
--     ( node
--     , Cfg.CodeReference
--         { function = ctx ^. #func
--         , startIndex = callInstr' ^. #index
--         , endIndex = callInstr' ^. #index
--         }
--     )
--   return node

-- nodeFromGroup :: Ctx -> InstrGroup -> NodeConverter MlilSsaCfNode
-- nodeFromGroup ctx = \case
--   (SingleCall x) -> nodeFromCallInstr ctx x
--   (ManyNonCalls xs) -> nodeFromInstrs ctx xs

-- -- | Parse a list of MlilSsaInstr into groups
-- groupInstrs :: [MlilSsaInstr] -> [InstrGroup]
-- groupInstrs xs = mconcat (parseAndSplit <$> groupedInstrs)
--  where
--   groupedInstrs :: [NonEmpty MlilSsaInstr]
--   groupedInstrs =
--     NEList.groupWith
--       ( \case
--           (MlilSsaCall _) -> True
--           (MlilSsaNonCall _) -> False
--       )
--       xs
--   parseCall :: MlilSsaInstr -> Maybe CallInstruction
--   parseCall = \case
--     (MlilSsaCall x') -> Just x'
--     _ -> error "Unexpected non-call instruction when parsing call instructions."
--   parseNonCall :: MlilSsaInstr -> Maybe NonCallInstruction
--   parseNonCall = \case
--     (MlilSsaNonCall x') -> Just x'
--     _ -> error "Unexpected call instruction when parsing non-call instructions."
--   -- Need to split consecutive calls
--   parseAndSplit :: NonEmpty MlilSsaInstr -> [InstrGroup]
--   parseAndSplit g = case g of
--     -- In this case we know all instructions in the group will be calls
--     -- based on the previous grouping performed.
--     (MlilSsaCall _) :| _ -> mapMaybe (fmap SingleCall . parseCall) $ NEList.toList g
--     -- This case handles non-call instructions. We know there are no calls
--     -- in the group.
--     _ -> [ManyNonCalls . NEList.fromList . mapMaybe parseNonCall . NEList.toList $ g]

-- nodesFromInstrs :: Ctx -> [MlilSsaInstruction] -> NodeConverter [MlilSsaCfNode]
-- nodesFromInstrs ctx instrs = do
--   let instrGroups = groupInstrs $ toMlilSsaInstr <$> instrs
--   mapM (nodeFromGroup ctx) instrGroups

-- convertNode :: Ctx -> MlilSsaBlock -> NodeConverter [MlilSsaCfNode]
-- convertNode ctx bnBlock = do
--   instrs <- liftIO $ Mlil.fromBasicBlock bnBlock
--   nodesFromInstrs ctx instrs

-- createEdgesForNodeGroup :: [MlilSsaCfNode] -> [MlilSsaCfEdge]
-- createEdgesForNodeGroup ns =
--   maybe [] (($ UnconditionalBranch) . uncurry CfEdge <$>) (maybeNodePairs =<< NEList.nonEmpty ns)
--  where
--   maybeNodePairs :: NonEmpty MlilSsaCfNode -> Maybe [(MlilSsaCfNode, MlilSsaCfNode)]
--   maybeNodePairs = \case
--     _ :| [] -> Nothing
--     nodes -> Just $ zip (NEList.toList nodes) (NEList.tail nodes)

-- convertBranchType :: BNEnums.BNBranchType -> BranchType
-- convertBranchType = \case
--   BNEnums.UnconditionalBranch -> UnconditionalBranch
--   BNEnums.TrueBranch -> TrueBranch
--   BNEnums.FalseBranch -> FalseBranch
--   _ -> UnconditionalBranch

-- convertEdge :: MlilSsaBlockMap -> MlilSsaBlockEdge -> Maybe MlilSsaCfEdge
-- convertEdge nodeMap bnEdge = do
--   let bnSrc = bnEdge ^. BNBb.src
--   bnDst <- bnEdge ^. BNBb.target
--   cfSrc <- lastMay =<< HMap.lookup bnSrc nodeMap
--   cfDst <- headMay =<< HMap.lookup bnDst nodeMap
--   return $ CfEdge cfSrc cfDst (convertBranchType $ bnEdge ^. BNBb.branchType)

-- importCfg ::
--   Function ->
--   CtxId ->
--   [MlilSsaBlock] ->
--   [MlilSsaBlockEdge] ->
--   IO (Maybe (ImportResult (Cfg MlilSsaCfNode) MlilNodeRefMap))
-- importCfg func currentCtxId bnNodes bnEdges = do
--   let ctx = Pil.Ctx func currentCtxId
--       nextCtxId' = currentCtxId + 1
--   (cfNodeGroups, mapEntries) <- runNodeConverter $ mapM (convertNode ctx) bnNodes
--   let mCfNodes = NEList.nonEmpty $ concat cfNodeGroups
--   case mCfNodes of
--     Nothing -> return Nothing
--     Just (cfRoot :| cfRest) -> do
--       let cfEdgesFromNodeGroups = concatMap createEdgesForNodeGroup cfNodeGroups
--           bnNodeMap = HMap.fromList $ zip bnNodes cfNodeGroups
--           cfEdgesFromBnCfg = convertEdge bnNodeMap <$> bnEdges
--           cfEdges = cfEdgesFromNodeGroups ++ catMaybes cfEdgesFromBnCfg
--       return
--         . Just
--         $ ImportResult ctx
--           (mkCfg nextCtxId' cfRoot cfRest cfEdges)
--           (HMap.fromList . DList.toList $ mapEntries)

-- isGotoBlock :: MlilSsaCfNode -> Bool
-- isGotoBlock (Cfg.BasicBlock bb) = NEList.length (bb ^. #nodeData) == 1 &&
--   case NEList.head (bb ^. #nodeData) ^. Mlil.op of
--     Mlil.GOTO _ -> True
--     _ -> False
-- isGotoBlock _ = False

-- removeGotoBlocks :: Cfg MlilSsaCfNode
--                  -> Cfg MlilSsaCfNode
-- removeGotoBlocks cfg = foldl' (flip Cfg.removeAndRebindEdges) cfg gotoNodes
--   where
--     gotoNodes = filter isGotoBlock . HashSet.toList . Cfg.nodes $ cfg

-- getCfgAlt :: BNBinaryView -> Function -> CtxId -> IO (Maybe (ImportResult (Cfg MlilSsaCfNode) MlilNodeRefMap))
-- getCfgAlt bv func currentCtxId = do
--   mBnFunc <- BNFunc.getFunctionStartingAt bv Nothing (func ^. #address)
--   case mBnFunc of
--     Nothing ->
--       return Nothing
--     (Just bnFunc) -> do
--       bnMlilFunc <- BNFunc.getMLILSSAFunction bnFunc
--       bnMlilBbs <- BNBb.getBasicBlocks bnMlilFunc
--       bnMlilBbEdges <- concatMapM BNBb.getOutgoingEdges bnMlilBbs
--       importCfg func currentCtxId bnMlilBbs bnMlilBbEdges

-- getCfg ::
--   (PilImporter a, IndexType a ~ MlilSsaInstructionIndex) =>
--   a ->
--   BNBinaryView ->
--   Function ->
--   CtxId ->
--   IO (Maybe (ImportResult PilCfg PilMlilNodeMap))
-- getCfg imp bv func currentCtxId = do
--   result <- getCfgAlt bv func currentCtxId
--   let ctx = Pil.Ctx func currentCtxId
--       nextCtxId' = currentCtxId + 1
--   case result of
--     Nothing -> return Nothing
--     Just (ImportResult _mlilCtx mlilCfgWithGotos mlilRefMapWithGotos) -> do
--       let mlilCfg = removeGotoBlocks mlilCfgWithGotos
--           mlilRefMap = HMap.filterWithKey (\k _ -> not $ isGotoBlock k) mlilRefMapWithGotos
--           mlilRootNode = Cfg.getRootNode mlilCfg
--           mlilRestNodes = HashSet.toList $ (HashSet.delete mlilRootNode . G.nodes) mlilCfg

--       pilRootNode <- convertToPilNode imp (ctx ^. #ctxId) mlilRefMap mlilRootNode
--       pilRestNodes <- traverse (convertToPilNode imp (ctx ^. #ctxId) mlilRefMap) mlilRestNodes
--       let mlilToPilNodeMap =
--             HMap.fromList $ zip (mlilRootNode : mlilRestNodes) (pilRootNode : pilRestNodes)
--           pilEdges = traverse (convertToPilEdge mlilToPilNodeMap) (Cfg.edges mlilCfg)
--           pilStmtsMap =
--             HMap.fromList $
--               ( (fromJust . (`HMap.lookup` mlilToPilNodeMap))
--                   *** identity
--               )
--                 <$> HMap.toList mlilRefMap
--       let mPilCfg = mkCfg nextCtxId' pilRootNode pilRestNodes <$> pilEdges
--       mPilCfg' <- traverse Cfg.splitTailCallNodes mPilCfg
--       return $
--         ImportResult ctx
--           <$> mPilCfg'
--           <*> Just pilStmtsMap


-- getPilFromNode ::
--   (PilImporter a, IndexType a ~ MlilSsaInstructionIndex) =>
--   a ->
--   CtxId ->
--   MlilNodeRefMap ->
--   MlilSsaCfNode ->
--   IO [Stmt]
-- getPilFromNode imp ctxId_ nodeMap node =
--   case HMap.lookup node nodeMap of
--     Nothing -> error $ "No entry for node: " <> show node <> "."
--     Just codeRef -> do
--       getCodeRefStatements imp ctxId_ codeRef

-- convertToPilNode ::
--   (PilImporter a, IndexType a ~ MlilSsaInstructionIndex) =>
--   a ->
--   CtxId ->
--   MlilNodeRefMap ->
--   MlilSsaCfNode ->
--   IO PilNode
-- convertToPilNode imp ctxId_ mapping mlilSsaNode = do
--   case mlilSsaNode of
--     BasicBlock (BasicBlockNode fun startAddr lastAddr _ _) -> do
--       stmts <- getPilFromNode imp ctxId_ mapping mlilSsaNode
--       uuid' <- randomIO
--       return $ BasicBlock (BasicBlockNode fun startAddr lastAddr uuid' stmts)
--     Call (CallNode fun startAddr _ _ _) -> do
--       stmts <- getPilFromNode imp ctxId_ mapping mlilSsaNode
--       let callDest = fromMaybe Pil.CallUnk $ do
--             stmt <- headMay stmts
--             callStmt <- Pil.mkCallStatement stmt
--             return $ Pil.getCallDest callStmt
--       uuid' <- randomIO
--       return $ Call (CallNode fun startAddr callDest uuid' stmts)
--     EnterFunc _ -> P.error "MLIL CFGs shouldn't have a EnterFunc node"
--     LeaveFunc _ -> P.error "MLIL CFGs shouldn't have a LeaveFunc node"
--     Grouping _ -> P.error "MLIL CFGs shouldn't have a Grouping node"

-- convertToPilEdge :: HashMap MlilSsaCfNode PilNode -> MlilSsaCfEdge -> Maybe PilEdge
-- convertToPilEdge nodeMap mlilSsaEdge =
--   CfEdge
--     <$> HMap.lookup (mlilSsaEdge ^. #src) nodeMap
--     <*> HMap.lookup (mlilSsaEdge ^. #dst) nodeMap
--     <*> Just (mlilSsaEdge ^. #branchType)
