module Blaze.Import.Source.BinaryNinja.Cfg where

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
  CfEdge (CfEdge),
  CfNode (
    BasicBlock,
    Call
  ),
  Cfg,
  PilCfg,
  PilEdge,
  PilNode,
  mkCfg,
  mkEdge,
 )
import qualified Blaze.Types.Cfg as Cfg
import Blaze.Types.Function (Function)
import Blaze.Types.Import (ImportResult (ImportResult))
import Blaze.Types.Pil (Stmt)
import Control.Monad.Trans.Writer.Lazy (runWriterT, tell)
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HMap
import qualified Data.List.NonEmpty as NEList
import qualified Data.Set as Set
import Data.Tuple.Extra ((***))

toMlilSsaInstr :: MlilSsaInstruction -> MlilSsaInstr
toMlilSsaInstr instr =
  maybe (MlilSsaNonCall $ NonCallInstruction instr) MlilSsaCall (toCallInstruction instr)

runNodeConverter :: NodeConverter a -> IO (a, DList MlilNodeRefMapEntry)
runNodeConverter = runWriterT

tellEntry :: MlilNodeRefMapEntry -> NodeConverter ()
tellEntry = tell . DList.singleton

-- | Assumes instructions are consecutive
nodeFromInstrs :: Function -> NonEmpty NonCallInstruction -> NodeConverter (CfNode (NonEmpty MlilSsaInstruction))
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

nodeFromCallInstr :: Function -> CallInstruction -> NodeConverter (CfNode (NonEmpty MlilSsaInstruction))
nodeFromCallInstr func' callInstr = do
  let node =
        Call $
          CallNode
            { function = func'
            , start = callInstr ^. #address
            , nodeData = callInstr ^. #instr :| []
            }
  tellEntry
    ( node
    , Cfg.CodeReference
        { function = func'
        , startIndex = callInstr ^. #index
        , endIndex = callInstr ^. #index
        }
    )
  return node

nodeFromGroup :: Function -> InstrGroup -> NodeConverter (CfNode (NonEmpty MlilSsaInstruction))
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

nodesFromInstrs :: Function -> [MlilSsaInstruction] -> NodeConverter [MlilSsaCfNode]
nodesFromInstrs func' instrs = do
  let instrGroups = groupInstrs $ toMlilSsaInstr <$> instrs
  mapM (nodeFromGroup func') instrGroups

convertNode :: Function -> MlilSsaBlock -> NodeConverter [MlilSsaCfNode]
convertNode func' bnBlock = do
  instrs <- liftIO $ Mlil.fromBasicBlock bnBlock
  nodesFromInstrs func' instrs

createEdgesForNodeGroup :: [MlilSsaCfNode] -> [MlilSsaCfEdge]
createEdgesForNodeGroup ns =
  maybe [] (($ UnconditionalBranch) . uncurry CfEdge <$>) (maybeNodePairs =<< NEList.nonEmpty ns)
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
  return $ CfEdge cfSrc cfDst (convertBranchType $ bnEdge ^. BNBb.branchType)

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
      return
        . Just
        $ ImportResult
          (mkCfg cfRoot cfRest cfEdges)
          (HMap.fromList . DList.toList $ mapEntries)

getCfgAlt :: BNBinaryView -> Function -> IO (Maybe (ImportResult (Cfg (NonEmpty MlilSsaInstruction)) MlilNodeRefMap))
getCfgAlt bv func' = do
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
  Function ->
  IO (Maybe (ImportResult PilCfg PilMlilNodeMap))
getCfg imp bv fun = do
  result <- getCfgAlt bv fun
  case result of
    Nothing -> return Nothing
    Just (ImportResult mlilCfg mlilRefMap) -> do
      let mlilRootNode = mlilCfg ^. #root
          mlilRestNodes = Set.toList $ (Set.delete mlilRootNode . G.nodes) mlilCfg
      pilRootNode <- convertToPilNode imp mlilRefMap mlilRootNode
      pilRestNodes <- traverse (convertToPilNode imp mlilRefMap) mlilRestNodes
      let mlilToPilNodeMap =
            HMap.fromList $ zip (mlilRootNode : mlilRestNodes) (pilRootNode : pilRestNodes)
          pilEdges = traverse (convertToPilEdge mlilToPilNodeMap . mkEdge) (G.edges mlilCfg)
          pilStmtsMap =
            HMap.fromList $
              ( (fromJust . (`HMap.lookup` mlilToPilNodeMap))
                  *** identity
              )
                <$> HMap.toList mlilRefMap
      return $
        ImportResult
          <$> (mkCfg pilRootNode pilRestNodes <$> pilEdges)
          <*> Just pilStmtsMap

getPilFromNode ::
  (PilImporter a, IndexType a ~ MlilSsaInstructionIndex) =>
  a ->
  MlilNodeRefMap ->
  CfNode (NonEmpty MlilSsaInstruction) ->
  IO [Stmt]
getPilFromNode imp nodeMap node =
  case HMap.lookup node nodeMap of
    Nothing -> error $ "No entry for node: " <> show node <> "."
    Just codeRef -> do
      getCodeRefStatements imp codeRef

convertToPilNode ::
  (PilImporter a, IndexType a ~ MlilSsaInstructionIndex) =>
  a ->
  MlilNodeRefMap ->
  MlilSsaCfNode ->
  IO PilNode
convertToPilNode imp mapping mlilSsaNode = do
  case mlilSsaNode of
    BasicBlock (BasicBlockNode fun startAddr lastAddr _) -> do
      stmts <- getPilFromNode imp mapping mlilSsaNode
      return $ BasicBlock (BasicBlockNode fun startAddr lastAddr stmts)
    Call (CallNode fun startAddr _) -> do
      stmts <- getPilFromNode imp mapping mlilSsaNode
      return $ Call (CallNode fun startAddr stmts)

convertToPilEdge :: HashMap MlilSsaCfNode PilNode -> MlilSsaCfEdge -> Maybe PilEdge
convertToPilEdge nodeMap mlilSsaEdge =
  CfEdge
    <$> HMap.lookup (mlilSsaEdge ^. #src) nodeMap
    <*> HMap.lookup (mlilSsaEdge ^. #dst) nodeMap
    <*> Just (mlilSsaEdge ^. #branchType)
