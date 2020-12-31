{-# LANGUAGE TemplateHaskell #-}

module Blaze.Import.Source.BinaryNinja (
  module Blaze.Import.Source.BinaryNinja,
--  module Exports,
) where
  
import qualified Binja.Core as Binja
import Binja.Core (BNBinaryView)
import Blaze.Import.CallGraph (CallGraphImporter (getCallSites, getFunctions, getFunction))
import Blaze.Import.Cfg (CfgImporter (getCfg))
import Blaze.Import.Pil (PilImporter (getFuncStatements, getPathStatements))
import Blaze.Prelude hiding (Symbol)
import Blaze.Import.Source.BinaryNinja.Types
import qualified Blaze.Import.Source.BinaryNinja.CallGraph as CallGraph
import qualified Blaze.Import.Source.BinaryNinja.Cfg as Cfg
import qualified Blaze.Import.Source.BinaryNinja.Pil as Pil
import qualified Blaze.Import.Source.BinaryNinja.Pil.Path as Path

newtype BNImporter
  = BNImporter
      { binaryView :: BNBinaryView
      }
  deriving (Eq, Ord, Show, Generic)

instance CallGraphImporter BNImporter where
  getFunction imp = CallGraph.getFunction (imp ^. #binaryView)
  
  getFunctions = CallGraph.getFunctions . view #binaryView

  getCallSites imp = CallGraph.getCallSites (imp ^. #binaryView)

instance CfgImporter BNImporter NodeMap where
  getCfg imp = Cfg.getCfg (imp ^. #binaryView)


instance PilImporter BNImporter where
  getFuncStatements imp =
    Pil.getFuncStatements (imp ^. #binaryView)

  getPathStatements imp =
    Path.runConvertPath (imp ^. #binaryView)


-- -- TODO: Have a proper Symbol type in BN bindings with fields already populated
-- --       so that IO is not needed here
-- convertSymbol :: Maybe BNSymbol -> IO (Maybe Symbol)
-- convertSymbol mbns = case mbns of
--   (Just bns) -> do
--     sname <- Text.pack <$> Binja.getSymbolFullName bns
--     srawname <- Text.pack <$> Binja.getSymbolRawName bns
--     return $
--       Just
--         Symbol
--           { _symbolName = sname,
--             _symbolRawName = srawname
--           }
--   Nothing -> return Nothing

-- convertFunction :: BNBinaryView -> BNFunc.Function -> IO CG.Function
-- convertFunction bv bnf = do
--   let name = bnf ^. BNFunc.name
--       address = bnf ^. BNFunc.start
--   symbol <- convertSymbol =<< Binja.View.getSymbolAtAddress bv address Nothing
--   return
--     CG.Function
--       { _functionSymbol = symbol,
--         _functionName = name,
--         _functionAddress = address
--       }

-- getCallInstruction :: BNFunc.Function -> Ref.ReferenceSource -> IO (Maybe CallInstruction)
-- getCallInstruction caller ref = do
--   llilFunc <- BNFunc.getLLILFunction caller
--   llilIndex <- Binja.getLLILInstructionIndexAtAddress caller (ref ^. Ref.arch) (ref ^. Ref.addr)
--   mlilIndex <- Mlil.getMLILFromLLIL llilFunc llilIndex
--   mlilFunc <- BNFunc.getMLILFunction caller
--   mlilSSAFunc <- BNFunc.getMLILSSAFunction caller
--   mlilSSAIndex <- Mlil.getMLILSSSAFromMLIL mlilFunc mlilIndex
--   toCallInstruction <$> Mlil.instruction mlilSSAFunc mlilSSAIndex

-- getCallDestAddr :: CallInstruction -> Maybe Address
-- getCallDestAddr ci =
--   case ci ^. Func.dest of
--     Just dexpr ->
--       case (dexpr ^. Mlil.op :: MLILSSAOp) of
--         (Mlil.CONST_PTR cpop) -> Just $ fromIntegral $ cpop ^. Mlil.constant
--         _ -> Nothing
--     _ -> Nothing

-- createCallSite :: BNBinaryView -> BNFunc.Function -> CallInstruction -> IO (Maybe CallSite)
-- createCallSite bv bnCaller callInstr = do
--   caller <- convertFunction bv bnCaller
--   let instrAddr = callInstr ^. Func.address
--       mDestAddr = getCallDestAddr callInstr
--   case mDestAddr of
--     Nothing -> return Nothing
--     Just addr -> do
--       mBnFunc <- BNFunc.getFunctionStartingAt bv Nothing addr
--       case mBnFunc of
--         Nothing -> return Nothing
--         Just bnFunc -> do
--           callee <- convertFunction bv bnFunc
--           return $
--             Just
--               ( CallSite
--                   { callSiteCaller = caller,
--                     callSiteAddress = instrAddr,
--                     callSiteDest = CG.DestFunc callee
--                   }
--               )

-- instance CallGraphImporter BNImporter where
--   getFunction imp addr = do
--     let bv = imp ^. binaryView
--     func <- BNFunc.getFunctionStartingAt bv Nothing addr :: IO (Maybe BNFunc.Function)
--     traverse (convertFunction bv) func

--   getFunctions imp = do
--     let bv = imp ^. binaryView
--     funcs <- BNFunc.getFunctions bv
--     traverse (convertFunction bv) funcs

--   getCallSites imp func = do
--     let bv = imp ^. binaryView
--     refs <- Ref.getCodeReferences bv (func ^. CG.address)
--     concatMapM (getCallSites' bv) refs
--     where
--       getMaybeCallerAndInstr :: BNFunc.Function -> Maybe CallInstruction -> Maybe (BNFunc.Function, CallInstruction)
--       getMaybeCallerAndInstr bnFunc maybeInstr = (,) <$> Just bnFunc <*> maybeInstr
--       getCallSites' :: BNBinaryView -> Ref.ReferenceSource -> IO [CG.CallSite]
--       getCallSites' bv ref = do
--         callers <- Set.toList <$> Binja.getFunctionsContaining bv (ref ^. Ref.addr)
--         mCallInstrs <- mapM (`getCallInstruction` ref) callers
--         -- TODO: Is there a better way to link non-Nothing callers and call instructions?
--         --       Right now we're fmap'ing a tuple constructor over them. This seems excessive.
--         let callSiteArgs = catMaybes $ uncurry getMaybeCallerAndInstr <$> zip callers mCallInstrs
--         mapMaybeM (uncurry $ createCallSite bv) callSiteArgs




-- ---- CFG

-- toMlilSsaInstr :: MlilSsaInstruction -> MlilSsaInstr
-- toMlilSsaInstr instr =
--   maybe (MlilSsaNonCall instr) MlilSsaCall (toCallInstruction instr)

-- runNodeConverter :: NodeConverter a -> IO (a, DList NodeMapEntry)
-- runNodeConverter = runWriterT

-- tellEntry :: NodeMapEntry -> NodeConverter ()
-- tellEntry = tell . DList.singleton

-- -- | Assumes instructions are consecutive
-- nodeFromInstrs :: Function -> NonEmpty NonCallInstruction -> NodeConverter CfNode
-- nodeFromInstrs func instrs = do
--   let node =
--         BasicBlock
--           { _basicBlockFunction = func,
--             _basicBlockStart = view Mlil.address . NEList.head $ instrs
--           }
--   tellEntry
--     ( node,
--       CodeReference
--         { _function = func,
--           _startIndex = view Mlil.index . NEList.head $ instrs,
--           _endIndex = view Mlil.index . NEList.last $ instrs
--         }
--     )
--   return node

-- nodeFromCallInstr :: Function -> CallInstruction -> NodeConverter CfNode
-- nodeFromCallInstr func callInstr = do
--   let node =
--         Call
--           { _callFunction = func,
--             _callTarget = callInstr ^. Func.address
--           }
--   tellEntry
--     ( node,
--       CodeReference
--         { _function = func,
--           _startIndex = callInstr ^. Func.index,
--           _endIndex = callInstr ^. Func.index
--         }
--     )
--   return node

-- nodeFromGroup :: Function -> InstrGroup -> NodeConverter CfNode
-- nodeFromGroup func = \case
--   (SingleCall x) -> nodeFromCallInstr func x
--   (ManyNonCalls xs) -> nodeFromInstrs func xs

-- -- | Parse a list of MlilSsaInstr into groups
-- groupInstrs :: [MlilSsaInstr] -> [InstrGroup]
-- groupInstrs xs = mconcat (parseAndSplit <$> groupedInstrs)
--   where
--     groupedInstrs :: [NonEmpty MlilSsaInstr]
--     groupedInstrs =
--       NEList.groupWith
--         ( \case
--             (MlilSsaCall _) -> True
--             (MlilSsaNonCall _) -> False
--         )
--         xs
--     parseCall :: MlilSsaInstr -> Maybe CallInstruction
--     parseCall = \case
--       (MlilSsaCall x') -> Just x'
--       _ -> error "Unexpected non-call instruction when parsing call instructions."
--     parseNonCall :: MlilSsaInstr -> Maybe NonCallInstruction
--     parseNonCall = \case
--       (MlilSsaNonCall x') -> Just x'
--       _ -> error "Unexpected call instruction when parsing non-call instructions."
--     -- Need to split consecutive calls
--     parseAndSplit :: NonEmpty MlilSsaInstr -> [InstrGroup]
--     parseAndSplit g = case g of
--       -- In this case we know all instructions in the group will be calls
--       -- based on the previous grouping performed.
--       (MlilSsaCall _) :| _ -> mapMaybe (fmap SingleCall . parseCall) $ NEList.toList g
--       -- This case handles non-call instructions. We know there are no calls
--       -- in the group.
--       _ -> [ManyNonCalls . NEList.fromList . mapMaybe parseNonCall . NEList.toList $ g]

-- nodesFromInstrs :: Function -> [MlilSsaInstruction] -> NodeConverter [CfNode]
-- nodesFromInstrs func instrs = do
--   let instrGroups = groupInstrs $ toMlilSsaInstr <$> instrs
--   mapM (nodeFromGroup func) instrGroups

-- convertNode :: Function -> MlilSsaBlock -> NodeConverter [CfNode]
-- convertNode func bnBlock = do
--   instrs <- liftIO $ Mlil.fromBasicBlock bnBlock
--   nodesFromInstrs func instrs

-- createEdgesForNodeGroup :: [CfNode] -> [CfEdge]
-- createEdgesForNodeGroup ns =
--   maybe [] (($ UnconditionalBranch) . uncurry CfEdge <$>) (maybeNodePairs =<< NEList.nonEmpty ns)
--   where
--     maybeNodePairs :: NonEmpty CfNode -> Maybe [(CfNode, CfNode)]
--     maybeNodePairs = \case
--       _ :| [] -> Nothing
--       nodes -> Just $ zip (NEList.toList nodes) (NEList.tail nodes)

-- convertBranchType :: BNEnums.BNBranchType -> BranchType
-- convertBranchType = \case
--   BNEnums.UnconditionalBranch -> UnconditionalBranch
--   BNEnums.TrueBranch -> TrueBranch
--   BNEnums.FalseBranch -> FalseBranch
--   _ -> UnconditionalBranch

-- convertEdge :: MlilSsaBlockMap -> MlilSsaBlockEdge -> Maybe CfEdge
-- convertEdge nodeMap bnEdge = do
--   let bnSrc = bnEdge ^. BNBb.src
--   bnDst <- bnEdge ^. BNBb.target
--   cfSrc <- lastMay =<< HMap.lookup bnSrc nodeMap
--   cfDst <- headMay =<< HMap.lookup bnDst nodeMap
--   return $ CfEdge cfSrc cfDst (convertBranchType $ bnEdge ^. BNBb.branchType)

-- importCfg ::
--   Function ->
--   [MlilSsaBlock] ->
--   [MlilSsaBlockEdge] ->
--   IO (Maybe (Cfg NodeMap))
-- importCfg func bnNodes bnEdges = do
--   (cfNodeGroups, mapEntries) <- runNodeConverter $ mapM (convertNode func) bnNodes
--   let mCfNodes = NEList.nonEmpty $ concat cfNodeGroups
--   case mCfNodes of
--     Nothing -> return Nothing
--     Just (cfRoot :| cfRest) -> do
--       let cfEdgesFromNodeGroups = concatMap createEdgesForNodeGroup cfNodeGroups
--           bnNodeMap = HMap.fromList $ zip bnNodes cfNodeGroups
--           cfEdgesFromBnCfg = convertEdge bnNodeMap <$> bnEdges
--           cfEdges = cfEdgesFromNodeGroups ++ catMaybes cfEdgesFromBnCfg
--       return . Just $ buildCfg cfRoot cfRest cfEdges (Just . HMap.fromList . DList.toList $ mapEntries)

-- instance CfgImporter BNImporter NodeMap where
--   getCfg imp func = do
--     let bv = imp ^. binaryView
--     mBnFunc <- BNFunc.getFunctionStartingAt bv Nothing (func ^. CG.address)
--     case mBnFunc of
--       Nothing ->
--         return Nothing
--       (Just bnFunc) -> do
--         bnMlilFunc <- BNFunc.getMLILSSAFunction bnFunc
--         bnMlilBbs <- BNBb.getBasicBlocks bnMlilFunc
--         bnMlilBbEdges <- concatMapM BNBb.getOutgoingEdges bnMlilBbs
--         importCfg func bnMlilBbs bnMlilBbEdges
