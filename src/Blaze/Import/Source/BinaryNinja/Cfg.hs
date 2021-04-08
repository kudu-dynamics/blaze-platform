module Blaze.Import.Source.BinaryNinja.Cfg where

import qualified Prelude as P
import qualified Binja.BasicBlock as BNBb
import qualified Binja.C.Enums as BNEnums
import Binja.Core (BNBinaryView)
import qualified Binja.Function as BNFunc
import qualified Binja.MLIL as Mlil
import qualified Blaze.Graph as G
import Blaze.Import.Pil (PilImporter (IndexType, getCodeRefStatements))
import Blaze.Import.Source.BinaryNinja.Types hiding (callDest, func)
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
    Call
  ),
  Cfg,
  PilCfg,
  PilEdge,
  PilNode,
  mkCfg,
 )
import qualified Blaze.Types.Cfg as Cfg
import Blaze.Types.Function (Function)
import Blaze.Types.Import (ImportResult (ImportResult))
import Blaze.Types.Pil (Stmt, CtxId, Ctx)
import qualified Blaze.Types.Pil as Pil
import Control.Monad.Trans.Writer.Lazy (runWriterT, tell)
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HMap
import qualified Data.List.NonEmpty as NEList
import qualified Data.Set as Set
import Data.Tuple.Extra ((***))

toMlilSsaInstr :: MlilSsaInstruction -> MlilSsaInstr
toMlilSsaInstr instr' =
  maybe (MlilSsaNonCall $ NonCallInstruction instr') MlilSsaCall (toCallInstruction instr')

runNodeConverter :: NodeConverter a -> IO (a, DList MlilNodeRefMapEntry)
runNodeConverter = runWriterT

tellEntry :: MlilNodeRefMapEntry -> NodeConverter ()
tellEntry = tell . DList.singleton

-- | Assumes instructions are consecutive
nodeFromInstrs :: Ctx -> NonEmpty NonCallInstruction -> NodeConverter (CfNode (NonEmpty MlilSsaInstruction))
nodeFromInstrs ctx instrs = do
  uuid' <- liftIO randomIO
  let node =
        BasicBlock $
          BasicBlockNode
            { ctx = ctx
            , start = (view Mlil.address . unNonCallInstruction) . NEList.head $ instrs
            , end = (view Mlil.address . unNonCallInstruction) . NEList.last $ instrs
            , nodeData = unNonCallInstruction <$> instrs
            , uuid = uuid'
            }
  tellEntry
    ( node
    , Cfg.CodeReference
        { function = ctx ^. #func
        , startIndex = view Mlil.index . unNonCallInstruction . NEList.head $ instrs
        , endIndex = view Mlil.index . unNonCallInstruction . NEList.last $ instrs
        }
    )
  return node

nodeFromCallInstr :: Ctx -> CallInstruction -> NodeConverter (CfNode (NonEmpty MlilSsaInstruction))
nodeFromCallInstr ctx callInstr' = do
  uuid' <- liftIO randomIO
  let node =
        Call $
          CallNode
            { ctx = ctx
            , start = callInstr' ^. #address
            , callDest = Pil.CallExpr ()
            , nodeData = callInstr' ^. #instr :| []
            , uuid = uuid'
            }
  tellEntry
    ( node
    , Cfg.CodeReference
        { function = ctx ^. #func
        , startIndex = callInstr' ^. #index
        , endIndex = callInstr' ^. #index
        }
    )
  return node

nodeFromGroup :: Ctx -> InstrGroup -> NodeConverter (CfNode (NonEmpty MlilSsaInstruction))
nodeFromGroup ctx = \case
  (SingleCall x) -> nodeFromCallInstr ctx x
  (ManyNonCalls xs) -> nodeFromInstrs ctx xs

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

nodesFromInstrs :: Ctx -> [MlilSsaInstruction] -> NodeConverter [MlilSsaCfNode]
nodesFromInstrs ctx instrs = do
  let instrGroups = groupInstrs $ toMlilSsaInstr <$> instrs
  mapM (nodeFromGroup ctx) instrGroups

convertNode :: Ctx -> MlilSsaBlock -> NodeConverter [MlilSsaCfNode]
convertNode ctx bnBlock = do
  instrs <- liftIO $ Mlil.fromBasicBlock bnBlock
  nodesFromInstrs ctx instrs

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
importCfg func bnNodes bnEdges = do
  ctx <- Pil.createCtx func
  (cfNodeGroups, mapEntries) <- runNodeConverter $ mapM (convertNode ctx) bnNodes
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
        $ ImportResult ctx
          (mkCfg cfRoot cfRest cfEdges)
          (HMap.fromList . DList.toList $ mapEntries)

removeGotoBlocks :: Cfg (NonEmpty MlilSsaInstruction) -> Cfg (NonEmpty MlilSsaInstruction)
removeGotoBlocks cfg = foldl' removeAndRebindEdges cfg gotoNodes
  where
    removeAndRebindEdges cfg' n =
      Cfg.removeNode n
      . Cfg.addEdges newEdges
      $ cfg'
      where
        preds = Set.toList $ Cfg.preds n cfg'
        succs = Set.toList $ Cfg.succs n cfg'
        newEdges = do
          pred <- preds
          succ <- succs
          return $ CfEdge pred succ Cfg.UnconditionalBranch

    isGotoBlock (Cfg.BasicBlock bb) = NEList.length (bb ^. #nodeData) == 1 &&
      case (NEList.head $ bb ^. #nodeData) ^. Mlil.op of
        Mlil.GOTO _ -> True
        _ -> False
    isGotoBlock _ = False
    
    gotoNodes = filter isGotoBlock . Set.toList . Cfg.nodes $ cfg

getCfgAlt :: BNBinaryView -> Function -> IO (Maybe (ImportResult (Cfg (NonEmpty MlilSsaInstruction)) MlilNodeRefMap))
getCfgAlt bv func = do
  mBnFunc <- BNFunc.getFunctionStartingAt bv Nothing (func ^. #address)
  case mBnFunc of
    Nothing ->
      return Nothing
    (Just bnFunc) -> do
      bnMlilFunc <- BNFunc.getMLILSSAFunction bnFunc
      bnMlilBbs <- BNBb.getBasicBlocks bnMlilFunc
      bnMlilBbEdges <- concatMapM BNBb.getOutgoingEdges bnMlilBbs
      importCfg func bnMlilBbs bnMlilBbEdges

getCfg ::
  (PilImporter a, IndexType a ~ MlilSsaInstructionIndex) =>
  a ->
  BNBinaryView ->
  Function ->
  IO (Maybe (ImportResult PilCfg PilMlilNodeMap))
getCfg imp bv func = do
  result <- getCfgAlt bv func
  ctx <- Pil.createCtx func
  case result of
    Nothing -> return Nothing
    Just (ImportResult _mlilCtx mlilCfg mlilRefMap) -> do
      let mlilRootNode = mlilCfg ^. #root
          mlilRestNodes = Set.toList $ (Set.delete mlilRootNode . G.nodes) mlilCfg
          mlilCfgSansGotos = removeGotoBlocks mlilCfg
      pilRootNode <- convertToPilNode imp (ctx ^. #ctxId)  mlilRefMap mlilRootNode
      pilRestNodes <- traverse (convertToPilNode imp (ctx ^. #ctxId) mlilRefMap) mlilRestNodes
      let mlilToPilNodeMap =
            HMap.fromList $ zip (mlilRootNode : mlilRestNodes) (pilRootNode : pilRestNodes)
          pilEdges = traverse (convertToPilEdge mlilToPilNodeMap) (Cfg.edges mlilCfgSansGotos)
          pilStmtsMap =
            HMap.fromList $
              ( (fromJust . (`HMap.lookup` mlilToPilNodeMap))
                  *** identity
              )
                <$> HMap.toList mlilRefMap
      return $
        ImportResult ctx
          <$> (mkCfg pilRootNode pilRestNodes <$> pilEdges)
          <*> Just pilStmtsMap
          

getPilFromNode ::
  (PilImporter a, IndexType a ~ MlilSsaInstructionIndex) =>
  a ->
  CtxId ->
  MlilNodeRefMap ->
  CfNode (NonEmpty MlilSsaInstruction) ->
  IO [Stmt]
getPilFromNode imp ctxId_ nodeMap node =
  case HMap.lookup node nodeMap of
    Nothing -> error $ "No entry for node: " <> show node <> "."
    Just codeRef -> do
      getCodeRefStatements imp ctxId_ codeRef

convertToPilNode ::
  (PilImporter a, IndexType a ~ MlilSsaInstructionIndex) =>
  a ->
  CtxId ->
  MlilNodeRefMap ->
  MlilSsaCfNode ->
  IO PilNode
convertToPilNode imp ctxId_ mapping mlilSsaNode = do
  case mlilSsaNode of
    BasicBlock (BasicBlockNode fun startAddr lastAddr _ _) -> do
      stmts <- getPilFromNode imp ctxId_ mapping mlilSsaNode
      uuid' <- randomIO
      return $ BasicBlock (BasicBlockNode fun startAddr lastAddr uuid' stmts)
    Call (CallNode fun startAddr _ _ _) -> do
      stmts <- getPilFromNode imp ctxId_ mapping mlilSsaNode
      let callDest = fromMaybe (Pil.CallExpr ()) $ do
            stmt <- headMay stmts
            callStmt <- Pil.mkCallStatement stmt
            return $ const () <$> Pil.getCallDest callStmt
      uuid' <- randomIO
      return $ Call (CallNode fun startAddr callDest uuid' stmts)
    Cfg.EnterFunc _ -> P.error "MLIL Cfg shouldn't have EnterFunc node"
    Cfg.LeaveFunc _ -> P.error "MLIL Cfg shouldn't have EnterFunc node"

convertToPilEdge :: HashMap MlilSsaCfNode PilNode -> MlilSsaCfEdge -> Maybe PilEdge
convertToPilEdge nodeMap mlilSsaEdge =
  CfEdge
    <$> HMap.lookup (mlilSsaEdge ^. #src) nodeMap
    <*> HMap.lookup (mlilSsaEdge ^. #dst) nodeMap
    <*> Just (mlilSsaEdge ^. #branchType)

