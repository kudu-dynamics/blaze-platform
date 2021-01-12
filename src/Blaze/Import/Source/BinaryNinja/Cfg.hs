module Blaze.Import.Source.BinaryNinja.Cfg where

import qualified Binja.BasicBlock as BNBb
import qualified Binja.C.Enums as BNEnums
import Binja.Core (BNBinaryView)
import qualified Binja.Function as BNFunc
import qualified Binja.MLIL as Mlil
import Blaze.Types.Function (CallInstruction, toCallInstruction)
import Blaze.Prelude hiding (Symbol)
import qualified Blaze.Types.CallGraph as CG
import Blaze.Types.CallGraph
  ( Function )
import qualified Blaze.Types.Cfg as Cfg
import Blaze.Types.Cfg
  ( BranchType
      ( FalseBranch,
        TrueBranch,
        UnconditionalBranch
      ),
    CfEdge (CfEdge),
    CfNode
      ( BasicBlock,
        Call
      ),
    Cfg,
    buildCfg,
  )
import Control.Monad.Trans.Writer.Lazy (runWriterT, tell)
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HMap
import qualified Data.List.NonEmpty as NEList
import Blaze.Import.Source.BinaryNinja.Types


toMlilSsaInstr :: MlilSsaInstruction -> MlilSsaInstr
toMlilSsaInstr instr =
  maybe (MlilSsaNonCall instr) MlilSsaCall (toCallInstruction instr)

runNodeConverter :: NodeConverter a -> IO (a, DList NodeMapEntry)
runNodeConverter = runWriterT

tellEntry :: NodeMapEntry -> NodeConverter ()
tellEntry = tell . DList.singleton

-- | Assumes instructions are consecutive
nodeFromInstrs :: Function -> NonEmpty NonCallInstruction -> NodeConverter CfNode
nodeFromInstrs func' instrs = do
  let node =
        BasicBlock
          { Cfg.function = func'
          , Cfg.start = view Mlil.address . NEList.head $ instrs
          }
  tellEntry
    ( node,
      CodeReference
        { function = func'
        , startIndex = view Mlil.index . NEList.head $ instrs
        , endIndex = view Mlil.index . NEList.last $ instrs
        }
    )
  return node

nodeFromCallInstr :: Function -> CallInstruction -> NodeConverter CfNode
nodeFromCallInstr func' callInstr = do
  let node =
        Call
          { function = func'
          , target = callInstr ^. #address
          }
  tellEntry
    ( node
    , CodeReference
        { function = func'
        , startIndex = callInstr ^. #index
        , endIndex = callInstr ^. #index
        }
    )
  return node

nodeFromGroup :: Function -> InstrGroup -> NodeConverter CfNode
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

nodesFromInstrs :: Function -> [MlilSsaInstruction] -> NodeConverter [CfNode]
nodesFromInstrs func' instrs = do
  let instrGroups = groupInstrs $ toMlilSsaInstr <$> instrs
  mapM (nodeFromGroup func') instrGroups

convertNode :: Function -> MlilSsaBlock -> NodeConverter [CfNode]
convertNode func' bnBlock = do
  instrs <- liftIO $ Mlil.fromBasicBlock bnBlock
  nodesFromInstrs func' instrs

createEdgesForNodeGroup :: [CfNode] -> [CfEdge]
createEdgesForNodeGroup ns =
  maybe [] (($ UnconditionalBranch) . uncurry CfEdge <$>) (maybeNodePairs =<< NEList.nonEmpty ns)
  where
    maybeNodePairs :: NonEmpty CfNode -> Maybe [(CfNode, CfNode)]
    maybeNodePairs = \case
      _ :| [] -> Nothing
      nodes -> Just $ zip (NEList.toList nodes) (NEList.tail nodes)

convertBranchType :: BNEnums.BNBranchType -> BranchType
convertBranchType = \case
  BNEnums.UnconditionalBranch -> UnconditionalBranch
  BNEnums.TrueBranch -> TrueBranch
  BNEnums.FalseBranch -> FalseBranch
  _ -> UnconditionalBranch

convertEdge :: MlilSsaBlockMap -> MlilSsaBlockEdge -> Maybe CfEdge
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
  IO (Maybe (Cfg NodeMap))
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
      return . Just $ buildCfg cfRoot cfRest cfEdges (Just . HMap.fromList . DList.toList $ mapEntries)

getCfg :: BNBinaryView -> CG.Function -> IO (Maybe (Cfg NodeMap))
getCfg bv func' = do
  mBnFunc <- BNFunc.getFunctionStartingAt bv Nothing (func' ^. #address)
  case mBnFunc of
    Nothing ->
      return Nothing
    (Just bnFunc) -> do
      bnMlilFunc <- BNFunc.getMLILSSAFunction bnFunc
      bnMlilBbs <- BNBb.getBasicBlocks bnMlilFunc
      bnMlilBbEdges <- concatMapM BNBb.getOutgoingEdges bnMlilBbs
      importCfg func' bnMlilBbs bnMlilBbEdges
