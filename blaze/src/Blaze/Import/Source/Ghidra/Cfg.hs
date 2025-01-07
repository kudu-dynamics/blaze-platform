module Blaze.Import.Source.Ghidra.Cfg where

import qualified Prelude as P

import Ghidra.Core (runGhidraOrError)
import qualified Ghidra.BasicBlock as BB
import Ghidra.State (GhidraState)
import qualified Ghidra.Function as GFunc
import qualified Ghidra.Pcode as Pcode
import qualified Ghidra.PcodeBlock as PB
import Ghidra.Types.Pcode.Lifted (PcodeOp)
import Ghidra.Program (getAddressSpaceMap)
import qualified Ghidra.Types.Pcode.Lifted as P
import qualified Ghidra.Types.Address as GAddr
import qualified Ghidra.Types.BasicBlock as GBB
import qualified Ghidra.Types.PcodeBlock as PB


import Ghidra.Types.Variable (HighVarNode, VarNode)
import qualified Ghidra.Types as J

import qualified Blaze.Import.Source.Ghidra.CallGraph as CGI
import Blaze.Import.Source.Ghidra.Types
import Blaze.Import.Source.Ghidra.Pil (IsVariable)
import qualified Blaze.Pil.Construct as C
import Blaze.Prelude hiding (Symbol, succ, pred)
import Blaze.Types.Cfg (
  BasicBlockNode (..),
  BranchType (..),
  CfEdge (..),
  CfNode (..),
  Cfg,
  MkCfgRootError(ZeroRootNodes, MultipleRootNodes),
  getNodeData,
  mkCfgFindRoot,
 )
import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Cfg as Cfg
import Blaze.Types.Function (Function)
import Blaze.Types.Import (ImportResult (ImportResult))
import Blaze.Types.Pil (CtxId, Ctx(Ctx), PilVar)
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Import.Source.Ghidra.Pil as PilConv

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NEList


newtype ConverterError
  = ExpectedConditionalOp Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

data ConverterState = ConverterState
  deriving (Show, Generic)

newtype Converter a = Converter { _runConverter :: ExceptT ConverterError (StateT ConverterState IO) a}
  deriving (Functor)
  deriving newtype (Applicative, Monad, MonadState ConverterState, MonadIO, MonadError ConverterError)

getRawPcodeForBasicBlock :: GhidraState -> GBB.BasicBlock -> IO [(Address, PcodeOp VarNode)]
getRawPcodeForBasicBlock gs bb = do
  xs <- runGhidraOrError $ do
    addrSpaceMap <- getAddressSpaceMap $ gs ^. #program
    Pcode.getRawPcode gs addrSpaceMap $ bb ^. #handle
  return $ first convertAddress <$> xs
  -- traverse (\(addr, op) -> (,op) . convertAddress <$> addr) xs

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

mkCfNodeBasicBlock :: (GBB.BasicBlock -> IO [(Address, PcodeOp a)]) -> Ctx -> GBB.BasicBlock -> IO (CfNode [(Address, PcodeOp a)])
mkCfNodeBasicBlock getPcode ctx bb = do
  uuid <- randomIO
  pcode <- getPcode bb
  let startAddr = convertAddress $ bb ^. #startAddress
  endAddr <- convertAddress <$> runGhidraOrError (BB.getMaxAddress bb)
  return . BasicBlock $ BasicBlockNode ctx startAddr endAddr uuid pcode

mkCfEdgesFromEdgeSet
  :: forall a. Show a
  => Function
  -> Map GBB.BasicBlock (CfNode [(Address, PcodeOp a)])
  -> (GBB.BasicBlock, [GBB.BasicBlock])
  -> [CfEdge (CfNode [(Address, PcodeOp a)])]
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
        Just (_addr, stmt) -> stmt

      -- | Returns True if start addr of basic block is CBranch dest
      checkTrue :: GBB.BasicBlock -> Bool
      checkTrue bb = case lastSrcStmt of
        P.CBRANCH inputDest _ -> case inputDest ^. #value of
          P.Absolute addr -> bb ^. #startAddress == addr
          P.Relative _ -> error "Unexpected relative address for CBRANCH"
        otherInstr -> do
          error $ "Unexpected instruction in " <> cs (fn ^. #name) <> ":\n" <> cs (pshow otherInstr)
  [] -> error "BasicBlock from EdgeSet has 0 outgoing edges" -- impossible
  xs -> error $ "Expected 1 or 2 edges, got " <> show (length xs)
  where
    src' = bbCfNodeMap Map.! src

type ThunkDestFunc = J.Function

getPcodeCfg
  :: (Show a, Hashable a)
  => (J.Function -> GhidraState -> GBB.BasicBlock -> IO [(Address, PcodeOp a)])
  -> GhidraState
  -> Function
  -> CtxId
  -> IO (Maybe (Either ThunkDestFunc (Cfg (CfNode [(Address, PcodeOp a)]))))
getPcodeCfg getPcode gs fn ctxId = do
  jfunc <- CGI.toGhidraFunction gs fn
  runGhidraOrError (GFunc.isThunk jfunc) >>= \case
    True -> do
      Just . Left <$> runGhidraOrError (GFunc.unsafeGetThunkedFunction True jfunc)
    False -> do
      bbGraph <- runGhidraOrError (BB.getBasicBlockGraph gs jfunc)
      let ctx = Ctx fn ctxId
      bbCfNodeTuples <- traverse (\bb -> (bb,) <$> mkCfNodeBasicBlock (getPcode jfunc gs) ctx bb)
        $ bbGraph ^. #nodes
      let bbCfNodeMap = Map.fromList bbCfNodeTuples
          edgeSets = getEdgeSets bbGraph
          cfEdges = concatMap (mkCfEdgesFromEdgeSet fn bbCfNodeMap) . Map.toList $ edgeSets
      case mkCfgFindRoot ctxId (snd <$> bbCfNodeTuples) cfEdges of
        Left err -> case err of
          ZeroRootNodes -> do
            warn $ "Cfg for '" <> show (fn ^. #name) <> "' has no root node"
            return Nothing
          MultipleRootNodes -> error "Cfg has more than one root node"
        Right cfg -> return . Just . Right $ cfg

getRawPcodeCfg
  :: GhidraState
  -> Function
  -> CtxId
  -> IO (Maybe (Either ThunkDestFunc (Cfg (CfNode [(Address, PcodeOp VarNode)]))))
getRawPcodeCfg = getPcodeCfg $ const getRawPcodeForBasicBlock


------------- High Pcode Block -----------------

getHighPcodeForPcodeBlock
  :: GhidraState
  -> PB.PcodeBlock
  -> IO [(Address, PcodeOp HighVarNode)]
getHighPcodeForPcodeBlock gs pb = do
  xs <- runGhidraOrError $ do
    addrSpaceMap <- getAddressSpaceMap $ gs ^. #program
    Pcode.getBlockHighPcode addrSpaceMap $ pb ^. #handle
  return $ first convertAddress <$> xs

mkCfNodePcodeBlock :: (PB.PcodeBlock -> IO [(Address, PcodeOp HighVarNode)]) -> Ctx -> PB.PcodeBlock -> IO (CfNode [(Address, PcodeOp HighVarNode)])
mkCfNodePcodeBlock getPcode ctx pb = do
  uuid <- randomIO
  pcode <- getPcode pb
  startAddr <- runGhidraOrError $ convertAddress <$> PB.getStart pb
  endAddr <- runGhidraOrError $ convertAddress <$> PB.getStop pb
  return . BasicBlock $ BasicBlockNode ctx startAddr endAddr uuid pcode

mkCfEdgeFromPcodeBlockEdge :: (PB.BranchType, (a, a)) -> CfEdge a
mkCfEdgeFromPcodeBlockEdge (bt, edges) = Cfg.fromTupleEdge $ case bt of
  PB.TrueBranch -> (Cfg.TrueBranch, edges)
  PB.FalseBranch -> (Cfg.FalseBranch, edges)
  _ -> (Cfg.UnconditionalBranch, edges)

getHighPcodeCfg
  :: GhidraState
  -> Function
  -> CtxId
  -> IO (Maybe (Either ThunkDestFunc (Cfg (CfNode [(Address, PcodeOp HighVarNode)]))))
getHighPcodeCfg gs fn ctxId = do
  jfunc <- CGI.toGhidraFunction gs fn
  runGhidraOrError (GFunc.isThunk jfunc) >>= \case
    True -> do
      Just . Left <$> runGhidraOrError (GFunc.unsafeGetThunkedFunction True jfunc)
    False ->  do
      hfunc <- runGhidraOrError $ GFunc.getHighFunction gs jfunc -- TODO: cache this
      bbGraph <- runGhidraOrError $ PB.getPcodeBlockGraph hfunc
      let ctx = Ctx fn ctxId
      nodePcodeTuples <- traverse (\pb -> (pb,) <$> mkCfNodePcodeBlock (getHighPcodeForPcodeBlock gs) ctx pb)
        $ bbGraph ^. #nodes
      let nodeMap = Map.fromList nodePcodeTuples
          bbGraph' = (nodeMap Map.!) <$> bbGraph
          edges' = mkCfEdgeFromPcodeBlockEdge <$> (bbGraph' ^. #edges)
          nodes' = snd <$> nodePcodeTuples
      case mkCfgFindRoot ctxId nodes' edges' of
        Left err -> case err of
          ZeroRootNodes -> do
            warn $ "Cfg for '" <> show (fn ^. #name) <> "' has no root node"
            return Nothing
          MultipleRootNodes -> error "Cfg has more than one root node"
        Right cfg -> return . Just . Right $ cfg


-------------- Convert Pcode CFG to PIL CFG --------------

-- | Converts a Pcode CFG to a PIL CFG.
convertToPilCfg
  :: (IsVariable a, Show a)
  => GhidraState
  -> Ctx
  -> Cfg (CfNode [(Address, PcodeOp a)])
  -> IO (HashMap PilVar VarNode, [PilConv.PCodeOpToPilStmtConversionError], Cfg (CfNode [Pil.Stmt]))
convertToPilCfg gs ctx cfg = do
  let convState = PilConv.mkConverterState gs ctx

  (r, cstate) <- flip PilConv.runConverter convState $ do
    traverse (traverse (fmap split . traverse (runExceptT . convertIndexedPcodeOpToPilStmt ctx))) cfg
  let pcfg = fmap snd <$> r
      errs = fold $ foldMap (toList . fmap fst) r
  return (cstate ^. #sourceVars, errs, pcfg)

  where
    split :: [Either a [b]] -> ([a], [b])
    split = second concat . partitionEithers

convertIndexedPcodeOpToPilStmt
  :: (IsVariable a, Show a)
  => Ctx
  -> (Address, PcodeOp a)
  -> ExceptT PilConv.PCodeOpToPilStmtConversionError PilConv.Converter [Pil.Stmt]
convertIndexedPcodeOpToPilStmt ctx (addr, op) = do
  modifyError
    (\e ->
      PilConv.PCodeOpToPilStmtConversionError
        { address = addr
        , function = ctx ^. #func
        , failedOp = show <$> op
        , conversionError = e
        })
  . fmap (fmap $ Pil.Stmt addr)
  $ PilConv.convertPcodeOpToPilStatement op
  where
    modifyError :: Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
    modifyError f m = ExceptT . fmap (first f) $ runExceptT m

splitCallsInCfg
  :: Ctx
  -> Cfg (CfNode [ Pil.Stmt])
  -> IO (Cfg (CfNode [Pil.Stmt]))
splitCallsInCfg ctx cfg = foldM splitAndReplace cfg . Cfg.nodes $ cfg
  where
    splitAndReplace
      :: Cfg (CfNode [Pil.Stmt])
      -> CfNode [Pil.Stmt]
      -> IO (Cfg (CfNode [Pil.Stmt]))
    splitAndReplace cfg' node = splitNodeOnCalls ctx node >>= \case
      Nothing -> return cfg'
      Just allNodesNE@(rootSplitNode :| splitNodes) -> do
        let allSplitNodes = rootSplitNode:splitNodes
            splitEdges = zipWith (\a b -> CfEdge a b Cfg.UnconditionalBranch) allSplitNodes splitNodes
            splitCfg = Cfg.mkCfg (ctx ^. #ctxId) rootSplitNode splitNodes splitEdges
        return $ Cfg.substNode cfg' node splitCfg $ NEList.last allNodesNE

-- | Splits basic blocks that have calls. Returns Nothing if no change was made.
splitNodeOnCalls
  :: Ctx
  -> CfNode [Pil.Stmt]
  -> IO (Maybe (NonEmpty (CfNode [Pil.Stmt])))
splitNodeOnCalls ctx' cnode = do
  xs <- groupNodes . fmap isCallOrNot . Cfg.getNodeData $ cnode
  return $ case xs of
    [] -> Nothing
    -- There's an 'if' and `==` here because an input CfNode could have a single
    -- call instr and thus be changed into a CallNode.
    [x] -> if x == cnode then Nothing else Just (NEList.singleton x)
    (y:ys) -> Just $ y :| ys
  where
    -- | Take non-call statements up to a Call or end. Returns:
    --   (addr of last stmt, non-call stmts, remaining list)
    takeWhileNotCall
      :: [Either Pil.Stmt Pil.CallStatement]
      -> Address
      -> (Address, [Pil.Stmt], [Either Pil.Stmt Pil.CallStatement])
    takeWhileNotCall [] lastAddr = (lastAddr, [], [])
    takeWhileNotCall xs@((Right _):_) lastAddr = (lastAddr, [], xs)
    takeWhileNotCall ((Left stmt@(Pil.Stmt addr _)):xs) _ = takeWhileNotCall xs addr & _2 %~ (stmt:)

    groupNodes :: [Either Pil.Stmt Pil.CallStatement] -> IO [CfNode [Pil.Stmt]]
    groupNodes [] = return []
    groupNodes ((Right cstmt):xs) = do
      uuid <- randomIO
      let callNode = Cfg.Call $ Cfg.CallNode
                     { ctx = ctx'
                     , start = cstmt ^. #stmt . #addr
                     , callDest = Pil.getCallDest cstmt
                     , uuid = uuid
                     , nodeData = [cstmt ^. #stmt]
                     }
      (callNode:) <$> groupNodes xs
    groupNodes (x@(Left (Pil.Stmt firstAddr _)):xs) = do
      let (lastAddr, nonCallStmts, restXs) = takeWhileNotCall (x:xs) firstAddr
      case nonCallStmts of
        [] -> groupNodes restXs
        _ -> do
          uuid <- randomIO
          let bbNode = Cfg.BasicBlock $ Cfg.BasicBlockNode
                       { ctx = ctx'
                       , start = firstAddr
                       , end = lastAddr
                       , uuid = uuid
                       , nodeData = nonCallStmts
                       }
          (bbNode:) <$> groupNodes restXs

    isCallOrNot :: Pil.Stmt -> Either Pil.Stmt Pil.CallStatement
    isCallOrNot stmt = maybe (Left stmt) Right $ Pil.mkCallStatement stmt

getPilCfg
  :: (IsVariable a, Show a)
  => (GhidraState -> Function -> CtxId -> IO (Maybe (Either ThunkDestFunc (Cfg (CfNode [(Address, PcodeOp a)])))))
  -> GhidraState
  -> Function
  -> CtxId
  -> IO (Maybe (ImportResult (PilPcodeMap VarNode) (Cfg (CfNode [Pil.Stmt]))))
getPilCfg pcodeCfgGetter gs func ctxId = do
  let ctx = Ctx func ctxId
  pcodeCfgGetter gs func ctxId >>= \case
    Nothing -> return Nothing
    Just (Left thunkedDest) -> do
      destName <- runGhidraOrError $ GFunc.getName thunkedDest
      runGhidraOrError (GFunc.isExternal thunkedDest) >>= \case
        True -> return Nothing
        False -> do
          callDest <- fmap Pil.CallFunc
            $ runGhidraOrError (GFunc.mkFunction thunkedDest)
            >>= CGI.toBlazeFunction
          (callNodeUuid, retNodeUuid) <- (,) <$> randomIO <*> randomIO
          let ctx' = Pil.Ctx func ctxId
              defaultRetSize = 8
              retVar = C.pilVar_ defaultRetSize (Just ctx) "rax"
              callNode = Cfg.Call $ Cfg.CallNode
                { ctx = ctx'
                , start = func ^. #address
                , callDest = callDest
                , uuid = callNodeUuid
                , nodeData =
                  [ Pil.Def
                    (Pil.DefOp retVar
                     (Pil.Expression defaultRetSize
                      (Pil.CALL (Pil.CallOp callDest (Just destName) []))))
                  ]
                }
              retNode = Cfg.BasicBlock $ Cfg.BasicBlockNode
                { ctx = ctx'
                , start = func ^. #address
                , end = func ^. #address
                , uuid = retNodeUuid
                , nodeData =
                  [ Pil.Ret . Pil.RetOp $ C.var' retVar defaultRetSize ]
                }
          let thunkCfg = Cfg.mkCfg ctxId callNode [callNode, retNode]
                [ Cfg.CfEdge callNode retNode Cfg.UnconditionalBranch ]
              indexedThunkCfg = fmap (fmap (Pil.Stmt $ func ^. #address)) <$> thunkCfg
      
          return . Just $ ImportResult ctx (PilPcodeMap HashMap.empty) indexedThunkCfg
    Just (Right pcodeCfg) -> do
      (varMapping, errs, pilCfg) <- convertToPilCfg gs ctx pcodeCfg
      unless (null errs) $ do
        putText $ "Warning: getPilCfg encountered errors for function: " <> show func
        traverse_ pprint errs
      splitPilCfg <- splitCallsInCfg ctx pilCfg
      return . Just $ ImportResult ctx (PilPcodeMap varMapping) splitPilCfg

removeStmtAddrs :: Cfg (CfNode [(Address, a)]) -> Cfg (CfNode [a])
removeStmtAddrs = fmap (fmap (fmap snd))

getPilCfgFromRawPcode
  :: GhidraState
  -> Function
  -> CtxId
  -> IO (Maybe (ImportResult (PilPcodeMap VarNode) (Cfg (CfNode [Pil.Stmt]))))
getPilCfgFromRawPcode = getPilCfg getRawPcodeCfg

getPilCfgFromHighPcode
  :: GhidraState
  -> Function
  -> CtxId
  -> IO (Maybe (ImportResult (PilPcodeMap VarNode) (Cfg (CfNode [Pil.Stmt]))))
getPilCfgFromHighPcode = getPilCfg getHighPcodeCfg
