module Blaze.Import.Source.BinaryNinja.Cfg where

import qualified Prelude as P
import qualified Binja.BasicBlock as BNBb
import qualified Binja.C.Enums as BNEnums
import Binja.Core (BNBinaryView)
import qualified Binja.Function as BNFunc
import qualified Binja.MLIL as Mlil
import qualified Blaze.Graph as G
import Blaze.Import.Pil (PilImporter (IndexType, getCodeRefStatements))
import Blaze.Import.Source.BinaryNinja.Types
import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Cfg (
  BasicBlockNode (BasicBlockNode),
  BranchType (
    FalseBranch,
    TrueBranch,
    UnconditionalBranch
  ),
  CallNode (CallNode),
  CfEdge,
  CfNodeWithId (CfNodeWithId),
  CfNode (
    BasicBlock,
    Call
  ),
  Cfg,
  PilCfg,
  PilEdge,
  PilNode,
  PilNodeType,
  mkCfg,
 )
import qualified Blaze.Types.Cfg as Cfg
import Blaze.Types.Function (Function)
import Blaze.Types.Import (ImportResult (ImportResult))
import Blaze.Types.Pil (Stmt, CtxIndex)
import Control.Monad.Trans.Writer.Lazy (runWriterT, tell)
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HMap
import qualified Data.List.NonEmpty as NEList
import qualified Data.Set as Set
import Data.Tuple.Extra ((***))
import qualified Data.List as List

toMlilSsaInstr :: MlilSsaInstruction -> MlilSsaInstr
toMlilSsaInstr instr' =
  maybe (MlilSsaNonCall $ NonCallInstruction instr') MlilSsaCall (toCallInstruction instr')

runNodeConverter :: NodeConverter a -> IO (a, DList MlilNodeRefMapEntry)
runNodeConverter = runWriterT

tellEntry :: MlilNodeRefMapEntry -> NodeConverter ()
tellEntry = tell . DList.singleton

-- | Assumes instructions are consecutive
nodeFromInstrs :: Function -> NonEmpty NonCallInstruction -> NodeConverter (NodeType (NonEmpty MlilSsaInstruction))
nodeFromInstrs func' instrs = do
  let node =
        BasicBlock $
          BasicBlockNode
            { function = func'
            , start = (view Mlil.address . unNonCallInstruction) . NEList.head $ instrs
            , end = (view Mlil.address . unNonCallInstruction) . NEList.last $ instrs
            , nodeData = unNonCallInstruction <$> instrs
            }
  tellEntry
    ( node
    , Cfg.CodeReference
        { function = func'
        , startIndex = view Mlil.index . unNonCallInstruction . NEList.head $ instrs
        , endIndex = view Mlil.index . unNonCallInstruction . NEList.last $ instrs
        }
    )
  return node

nodeFromCallInstr :: Function -> CallInstruction -> NodeConverter (NodeType (NonEmpty MlilSsaInstruction))
nodeFromCallInstr func' callInstr' = do
  let node =
        Call $
          CallNode
            { function = func'
            , start = callInstr' ^. #address
            , nodeData = callInstr' ^. #instr :| []
            }
  tellEntry
    ( node
    , Cfg.CodeReference
        { function = func'
        , startIndex = callInstr' ^. #index
        , endIndex = callInstr' ^. #index
        }
    )
  return node

nodeFromGroup :: Function -> InstrGroup -> NodeConverter (NodeType (NonEmpty MlilSsaInstruction))
nodeFromGroup func' = \case
  (SingleCall x) -> nodeFromCallInstr func' x
  (ManyNonCalls xs) -> nodeFromInstrs func' xs

-- | Parse a list of MlilSsaInstr into groups
groupInstrs :: [MlilSsaInstr] -> [InstrGroup]
groupInstrs xs = mconcat (parseAndSplit <$> groupedInstrs)
 where
  groupedInstrs :: [NonEmpty MlilSsaInstr]
  groupedInstrs =
    NEList.groupWith
      ( \case
          (MlilSsaCall _) -> True
          (MlilSsaNonCall _) -> False
      )
      xs
  parseCall :: MlilSsaInstr -> Maybe CallInstruction
  parseCall = \case
    (MlilSsaCall x') -> Just x'
    _ -> error "Unexpected non-call instruction when parsing call instructions."
  parseNonCall :: MlilSsaInstr -> Maybe NonCallInstruction
  parseNonCall = \case
    (MlilSsaNonCall x') -> Just x'
    _ -> error "Unexpected call instruction when parsing non-call instructions."
  -- Need to split consecutive calls
  parseAndSplit :: NonEmpty MlilSsaInstr -> [InstrGroup]
  parseAndSplit g = case g of
    -- In this case we know all instructions in the group will be calls
    -- based on the previous grouping performed.
    (MlilSsaCall _) :| _ -> mapMaybe (fmap SingleCall . parseCall) $ NEList.toList g
    -- This case handles non-call instructions. We know there are no calls
    -- in the group.
    _ -> [ManyNonCalls . NEList.fromList . mapMaybe parseNonCall . NEList.toList $ g]

nodesFromInstrs :: Function -> [MlilSsaInstruction] -> NodeConverter [MlilSsaCfNodeType]
nodesFromInstrs func' instrs = do
  let instrGroups = groupInstrs $ toMlilSsaInstr <$> instrs
  mapM (nodeFromGroup func') instrGroups

convertNode :: Function -> MlilSsaBlock -> NodeConverter [MlilSsaCfNodeType]
convertNode func' bnBlock = do
  instrs <- liftIO $ Mlil.fromBasicBlock bnBlock
  nodesFromInstrs func' instrs

createEdgesForNodeGroup :: [MlilSsaCfNodeType] -> [MlilSsaCfEdge]
createEdgesForNodeGroup ns =
  maybe [] (($ UnconditionalBranch) . uncurry G.Edge <$>) (maybeNodePairs =<< NEList.nonEmpty ns)
 where
  maybeNodePairs :: NonEmpty MlilSsaCfNode -> Maybe [(MlilSsaCfNode, MlilSsaCfNode)]
  maybeNodePairs = \case
    _ :| [] -> Nothing
    nodes -> Just $ zip (NEList.toList nodes) (NEList.tail nodes)

convertBranchType :: BNEnums.BNBranchType -> BranchType
convertBranchType = \case
  BNEnums.UnconditionalBranch -> UnconditionalBranch
  BNEnums.TrueBranch -> TrueBranch
  BNEnums.FalseBranch -> FalseBranch
  _ -> UnconditionalBranch

convertEdge :: MlilSsaBlockMap -> MlilSsaBlockEdge -> Maybe MlilSsaCfEdge
convertEdge nodeMap bnEdge = do
  let bnSrc = bnEdge ^. BNBb.src
  bnDst <- bnEdge ^. BNBb.target
  cfSrc <- lastMay =<< HMap.lookup bnSrc nodeMap
  cfDst <- headMay =<< HMap.lookup bnDst nodeMap
  return $ Cfg.mkLEdge (convertBranchType $ bnEdge ^. BNBb.branchType) cfSrc cfDst

importCfg ::
  Function ->
  [MlilSsaBlock] ->
  [MlilSsaBlockEdge] ->
  IO (Maybe (ImportResult (Cfg (NonEmpty MlilSsaInstruction)) MlilNodeRefMap))
importCfg func' bnNodes bnEdges = do
  (cfNodeGroups, mapEntries) <- runNodeConverter $ mapM (convertNode func') bnNodes
  let mCfNodes = NEList.nonEmpty $ concat cfNodeGroups
  case mCfNodes of
    Nothing -> return Nothing
    Just (cfRoot :| cfRest) -> do
      let cfEdgesFromNodeGroups = concatMap createEdgesForNodeGroup cfNodeGroups
          bnNodeMap = HMap.fromList $ zip bnNodes cfNodeGroups
          cfEdgesFromBnCfg = convertEdge bnNodeMap <$> bnEdges
          cfEdges = cfEdgesFromNodeGroups ++ catMaybes cfEdgesFromBnCfg
      cfg <- mkCfg cfRoot cfRest cfEdges
      return
        . Just
        $ ImportResult cfg (HMap.fromList . DList.toList $ mapEntries)

getCfgAlt :: BNBinaryView -> CtxIndex -> Function -> IO (Maybe (ImportResult (Cfg (NonEmpty MlilSsaInstruction)) MlilNodeRefMap))
getCfgAlt bv _ctxIndex func' = do
  mBnFunc <- BNFunc.getFunctionStartingAt bv Nothing (func' ^. #address)
  case mBnFunc of
    Nothing ->
      return Nothing
    (Just bnFunc) -> do
      bnMlilFunc <- BNFunc.getMLILSSAFunction bnFunc
      bnMlilBbs <- BNBb.getBasicBlocks bnMlilFunc
      bnMlilBbEdges <- concatMapM BNBb.getOutgoingEdges bnMlilBbs
      importCfg func' bnMlilBbs bnMlilBbEdges

getCfg ::
  (PilImporter a, IndexType a ~ MlilSsaInstructionIndex) =>
  a ->
  BNBinaryView ->
  CtxIndex ->
  Function ->
  IO (Maybe (ImportResult PilCfg PilMlilNodeMap))
getCfg imp bv ctxIndex_ fun = do
  result <- getCfgAlt bv ctxIndex_ fun
  case result of
    Nothing -> return Nothing
    Just (ImportResult mlilCfg mlilRefMap) -> do
      let mlilRootNode = Cfg.getRoot mlilCfg
          mlilRestNodes = List.delete mlilRootNode . Cfg.nodes $ mlilCfg
      pilRootNodeType <- convertToPilNodeType imp ctxIndex_ mlilRefMap mlilRootNode
      pilRestNodeTypes <- traverse (convertToPilNodeType imp ctxIndex_ mlilRefMap) mlilRestNodes
      let mlilToPilNodeTypeMap =
            HMap.fromList $ zip (mlilRootNode : mlilRestNodes) (pilRootNodeType : pilRestNodeTypes)
          pilEdges = fromMaybe [] . traverse (convertToPilEdge mlilToPilNodeTypeMap) $ Cfg.edges mlilCfg
          pilStmtsMap =
            HMap.fromList $
              ( (fromJust . (`HMap.lookup` mlilToPilNodeTypeMap))
                  *** identity
              )
              <$> HMap.toList mlilRefMap
      cfg <- Cfg.mkCfg pilRootNodeType pilRestNodeTypes pilEdges
      return undefined
      -- return $
      --   ImportResult
      --     <$> cfg
      --     <*> Just pilStmtsMap

getPilFromNode ::
  (PilImporter a, IndexType a ~ MlilSsaInstructionIndex) =>
  a ->
  CtxIndex ->
  MlilNodeRefMap ->
  CfNode (NonEmpty MlilSsaInstruction) ->
  IO [Stmt]
getPilFromNode imp ctxIndex_ nodeMap node =
  case HMap.lookup node nodeMap of
    Nothing -> error $ "No entry for node: " <> show node <> "."
    Just codeRef -> do
      getCodeRefStatements imp ctxIndex_ codeRef

convertToPilNodeType ::
  (PilImporter a, IndexType a ~ MlilSsaInstructionIndex) =>
  a ->
  CtxIndex ->
  MlilNodeRefMap ->
  MlilSsaCfNode ->
  IO PilNodeType
convertToPilNodeType imp ctxIndex_ mapping mlilSsaNode = do
  case mlilSsaNode ^. #nodeType of
    BasicBlock (BasicBlockNode fun startAddr lastAddr _) -> do
      stmts <- getPilFromNode imp ctxIndex_ mapping mlilSsaNode
      return $ BasicBlock (BasicBlockNode fun startAddr lastAddr stmts)
    Call (CallNode fun startAddr _) -> do
      stmts <- getPilFromNode imp ctxIndex_ mapping mlilSsaNode
      return $ Call (CallNode fun startAddr stmts)
    Cfg.EnterFunc _ -> P.error "MLIL Cfg shouldn't have EnterFunc node"
    Cfg.LeaveFunc _ -> P.error "MLIL Cfg shouldn't have EnterFunc node"

convertToPilEdge :: HashMap MlilSsaCfNode PilNodeType -> MlilSsaCfEdge -> Maybe (G.LEdge BranchType (NodeType [Stmt]))
convertToPilEdge nodeMap mlilSsaEdge =
  G.LEdge (mlilSsaEdge ^. #label)
  <$> (G.Edge <$> HMap.lookup (mlilSsaEdge ^. #edge . #src) nodeMap
              <*> HMap.lookup (mlilSsaEdge ^. #edge . #dst) nodeMap)
