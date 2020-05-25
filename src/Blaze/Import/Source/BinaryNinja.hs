{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Import.BinaryNinja where

import qualified Binja.BasicBlock as BNBb
import qualified Binja.C.Enums as BNEnums
import qualified Binja.Core as Binja
import Binja.Core (BNBinaryView, BNSymbol)
import qualified Binja.Function as BNFunc
import qualified Binja.MLIL as Mlil
import qualified Binja.Reference as Ref
import qualified Binja.View
import Blaze.Function (CallInstruction, toCallInstruction)
import qualified Blaze.Function as Func
import Blaze.Import.CallGraph (CallGraphImporter (getCallSites, getFunctions))
import Blaze.Import.Cfg (CfgImporter (getCfg))
import Blaze.Prelude hiding (Symbol)
import qualified Blaze.Types.CallGraph as CG
import Blaze.Types.CallGraph
  ( CallSite
      ( CallSite,
        _callSiteAddress,
        _callSiteCaller,
        _callSiteDest
      ),
    Function,
  )
import Blaze.Types.Cfg
  ( BranchType
      ( FalseBranch,
        TrueBranch,
        UnconditionalBranch
      ),
    CfEdge (CfEdge),
    CfNode
      ( BasicBlock,
        Call,
        _basicBlockFunction,
        _basicBlockStart,
        _callFunction,
        _callTarget
      ),
    Cfg,
    buildCfg,
  )
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Trans.Writer.Lazy (WriterT, runWriterT, tell)
import Data.BinaryAnalysis (Address, Symbol (Symbol, _symbolName, _symbolRawName))
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HMap
import qualified Data.List.NonEmpty as NEList
import qualified Data.Set as Set
import qualified Data.Text as Text
import Prelude (error)

newtype BNImporter
  = BNImporter
      { _binaryView :: BNBinaryView
      }
  deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''BNImporter)

-- TODO: Have a proper Symbol type in BN bindings with fields already populated
--       so that IO is not needed here
convertSymbol :: Maybe BNSymbol -> IO (Maybe Symbol)
convertSymbol mbns = case mbns of
  (Just bns) -> do
    sname <- Text.pack <$> Binja.getSymbolFullName bns
    srawname <- Text.pack <$> Binja.getSymbolRawName bns
    return $
      Just
        Symbol
          { _symbolName = sname,
            _symbolRawName = srawname
          }
  Nothing -> return Nothing

convertFunction :: BNBinaryView -> BNFunc.Function -> IO CG.Function
convertFunction bv bnf = do
  let name = bnf ^. BNFunc.name
      address = bnf ^. BNFunc.start
  symbol <- convertSymbol =<< Binja.View.getSymbolAtAddress bv address Nothing
  return
    CG.Function
      { _functionSymbol = symbol,
        _functionName = name,
        _functionAddress = address
      }

getCallInstruction :: BNFunc.Function -> Ref.ReferenceSource -> IO (Maybe CallInstruction)
getCallInstruction caller ref = do
  llilFunc <- BNFunc.getLLILFunction caller
  llilIndex <- Binja.getLLILInstructionIndexAtAddress caller (ref ^. Ref.arch) (ref ^. Ref.addr)
  mlilIndex <- Mlil.getMLILFromLLIL llilFunc llilIndex
  mlilFunc <- BNFunc.getMLILFunction caller
  mlilSSAFunc <- BNFunc.getMLILSSAFunction caller
  mlilSSAIndex <- Mlil.getMLILSSSAFromMLIL mlilFunc mlilIndex
  toCallInstruction <$> Mlil.instruction mlilSSAFunc mlilSSAIndex

type MLILSSAOp = Mlil.Operation (Mlil.Expression BNFunc.MLILSSAFunction)

getCallDestAddr :: CallInstruction -> Maybe Address
getCallDestAddr ci =
  case ci ^. Func.dest of
    Just dexpr ->
      case (dexpr ^. Mlil.op :: MLILSSAOp) of
        (Mlil.CONST_PTR cpop) -> Just $ fromIntegral $ cpop ^. Mlil.constant
        _ -> Nothing
    _ -> Nothing

createCallSite :: BNBinaryView -> BNFunc.Function -> CallInstruction -> IO (Maybe CallSite)
createCallSite bv bnCaller callInstr = do
  caller <- convertFunction bv bnCaller
  let instrAddr = callInstr ^. Func.address
      mDestAddr = getCallDestAddr callInstr
  case mDestAddr of
    Nothing -> return Nothing
    Just addr -> do
      mBnFunc <- BNFunc.getFunctionStartingAt bv Nothing addr
      case mBnFunc of
        Nothing -> return Nothing
        Just bnFunc -> do
          callee <- convertFunction bv bnFunc
          return $
            Just
              ( CallSite
                  { _callSiteCaller = caller,
                    _callSiteAddress = instrAddr,
                    _callSiteDest = CG.DestFunc callee
                  }
              )

instance CallGraphImporter BNImporter where
  getFunctions imp = do
    let bv = imp ^. binaryView
    funcs <- BNFunc.getFunctions bv
    sequence $ convertFunction bv <$> funcs

  getCallSites imp func = do
    let bv = imp ^. binaryView
    refs <- Ref.getCodeReferences bv (func ^. CG.address)
    concatMapM (getCallSites' bv) refs
    where
      getMaybeCallerAndInstr :: BNFunc.Function -> Maybe CallInstruction -> Maybe (BNFunc.Function, CallInstruction)
      getMaybeCallerAndInstr bnFunc maybeInstr = (,) <$> Just bnFunc <*> maybeInstr
      getCallSites' :: BNBinaryView -> Ref.ReferenceSource -> IO [CG.CallSite]
      getCallSites' bv ref = do
        callers <- Set.toList <$> Binja.getFunctionsContaining bv (ref ^. Ref.addr)
        mCallInstrs <- mapM (`getCallInstruction` ref) callers
        -- TODO: Is there a better way to link non-Nothing callers and call instructions?
        --       Right now we're fmap'ing a tuple constructor over them. This seems excessive.
        let callSiteArgs = catMaybes $ uncurry getMaybeCallerAndInstr <$> zip callers mCallInstrs
        mapMaybeM (uncurry $ createCallSite bv) callSiteArgs

---- CFG
type MlilSsaFunc = BNFunc.MLILSSAFunction

type MlilSsaBlock = BNBb.BasicBlock MlilSsaFunc

type MlilSsaBlockEdge = BNBb.BlockEdge MlilSsaFunc

type MlilSsaInstruction = Mlil.Instruction MlilSsaFunc

type MlilSsaInstructionIndex = Binja.InstructionIndex MlilSsaFunc

type MlilSsaBlockMap = HashMap MlilSsaBlock [CfNode]

type NonCallInstruction = MlilSsaInstruction

data InstrGroup
  = SingleCall {_callInstr :: CallInstruction}
  | ManyNonCalls {_nonCallInstrs :: NonEmpty NonCallInstruction}
  deriving (Eq, Ord, Show)

data MlilSsaInstr
  = MlilSsaCall CallInstruction
  | MlilSsaNonCall NonCallInstruction
  deriving (Eq, Ord, Show)

toMlilSsaInstr :: MlilSsaInstruction -> MlilSsaInstr
toMlilSsaInstr instr =
  maybe (MlilSsaNonCall instr) MlilSsaCall (toCallInstruction instr)

data CodeReference
  = CodeReference
      { _function :: Function,
        _startIndex :: MlilSsaInstructionIndex,
        _endIndex :: MlilSsaInstructionIndex
      }
  deriving (Eq, Ord, Show)

type NodeMap = HashMap CfNode CodeReference

type NodeMapEntry = (CfNode, CodeReference)

type NodeConverter a = WriterT (DList NodeMapEntry) IO a

runNodeConverter :: NodeConverter a -> IO (a, DList NodeMapEntry)
runNodeConverter = runWriterT

tellEntry :: NodeMapEntry -> NodeConverter ()
tellEntry = tell . DList.singleton

-- | Assumes instructions are consecutive
nodeFromInstrs :: Function -> NonEmpty NonCallInstruction -> NodeConverter CfNode
nodeFromInstrs func instrs = do
  let node =
        BasicBlock
          { _basicBlockFunction = func,
            _basicBlockStart = (^. Mlil.address) . NEList.head $ instrs
          }
  tellEntry
    ( node,
      CodeReference
        { _function = func,
          _startIndex = (^. Mlil.index) . NEList.head $ instrs,
          _endIndex = (^. Mlil.index) . NEList.last $ instrs
        }
    )
  return node

nodeFromCallInstr :: Function -> CallInstruction -> NodeConverter CfNode
nodeFromCallInstr func callInstr = do
  let node =
        Call
          { _callFunction = func,
            _callTarget = callInstr ^. Func.address
          }
  tellEntry
    ( node,
      CodeReference
        { _function = func,
          _startIndex = callInstr ^. Func.index,
          _endIndex = callInstr ^. Func.index
        }
    )
  return node

nodeFromGroup :: Function -> InstrGroup -> NodeConverter CfNode
nodeFromGroup func = \case
  (SingleCall x) -> nodeFromCallInstr func x
  (ManyNonCalls xs) -> nodeFromInstrs func xs

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
nodesFromInstrs func instrs = do
  let instrGroups = groupInstrs $ toMlilSsaInstr <$> instrs
  mapM (nodeFromGroup func) instrGroups

convertNode :: Function -> MlilSsaBlock -> NodeConverter [CfNode]
convertNode func bnBlock = do
  instrs <- liftIO $ Mlil.fromBasicBlock bnBlock
  nodesFromInstrs func instrs

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
  IO (Cfg NodeMap)
importCfg func bnNodes bnEdges = do
  (cfNodeGroups, mapEntries) <- runNodeConverter $ mapM (convertNode func) bnNodes
  let cfEdgesFromNodeGroups = concatMap createEdgesForNodeGroup cfNodeGroups
      bnNodeMap = HMap.fromList $ zip bnNodes cfNodeGroups
      cfEdgesFromBnCfg = convertEdge bnNodeMap <$> bnEdges
      cfEdges = cfEdgesFromNodeGroups ++ catMaybes cfEdgesFromBnCfg
  return $ buildCfg (concat cfNodeGroups) cfEdges (Just . HMap.fromList . DList.toList $ mapEntries)

instance CfgImporter BNImporter NodeMap where
  getCfg imp func = do
    let bv = imp ^. binaryView
    mBnFunc <- BNFunc.getFunctionStartingAt bv Nothing (func ^. CG.address)
    case mBnFunc of
      Nothing ->
        return Nothing
      (Just bnFunc) -> do
        bnMlilFunc <- BNFunc.getMLILSSAFunction bnFunc
        bnMlilBbs <- BNBb.getBasicBlocks bnMlilFunc
        bnMlilBbEdges <- concatMapM BNBb.getOutgoingEdges bnMlilBbs
        Just <$> importCfg func bnMlilBbs bnMlilBbEdges
