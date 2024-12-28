{- HLINT ignore "Use <$>" -}
module Blaze.Cfg.Path
 ( module Blaze.Cfg.Path
 , module Exports
 ) where

import Blaze.Prelude hiding (Symbol)

import Blaze.Types.Cfg (Cfg, CfNode, CallNode, HasCtx(getCtx), BranchType(UnconditionalBranch), setNodeUUID, PilNode)
import qualified Blaze.Cfg as Cfg
import Blaze.Types.Cfg.Grouping (unfoldGroups)
import Blaze.Types.Cfg.Path as Exports
import qualified Blaze.Types.Graph as G
import Blaze.Types.Graph (StrictDescendantsMap)
import Blaze.Cfg.Interprocedural ()
import qualified Blaze.Cfg.Interprocedural as CfgI
import qualified Blaze.Path as P
import Blaze.Path (SampleRandomPathError, SampleRandomPathError', ChildChooser, withNestedState)
import qualified Blaze.Pil.Analysis as PA
import Blaze.Types.Pil (Stmt, CallStatement, Expression, PilVar, Symbol)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil.Analysis.Subst (RecurSubst(recurSubst), FlatSubst(flatSubst))
import qualified Blaze.Pil.Construct as C
import qualified Blaze.Cfg.Loop as Loop

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List (nub)


-- | Generates all paths from a Cfg that do not visit the same node twice.
-- This may include paths that end in the body of a loop and thus
-- do not have a return node.
-- Calls unfoldGroups on Cfg to ensure there are no groups.
-- Note: Loop summary nodes might one day enable us to return only returning paths.
getAllSimplePaths :: (Ord a, Hashable a) => Cfg (CfNode a) -> [Path (CfNode a)]
getAllSimplePaths cfg = mkCfPath <$> P.getAllPaths (\_ _ -> error "should not revisit") 0
  (Cfg.getRootNode cfg')
  (cfg' ^. #graph)
  where
    nextCtxIndex_ = cfg ^. #nextCtxIndex
    outerCtx_ = getCtx cfg'
    mkCfPath = Path nextCtxIndex_ outerCtx_
    (cfg', _) = unfoldGroups cfg


-- | Generates all paths from a Cfg that do not visit the same node twice.
-- This may include paths that end in the body of a loop and thus
-- do not have a return node.
-- Returns only paths containing all the nodes specified in `reqNodes`
-- Calls unfoldGroups on Cfg to ensure there are no groups.
getSimplePathsContaining
  :: (Ord a, Hashable a)
  => HashSet (CfNode a)
  -> Cfg (CfNode a)
  -> [Path (CfNode a)]
getSimplePathsContaining reqNodes cfg = mkCfPath <$> P.getPathsContaining (\_ _ -> error "should not revisit") 0
  (Cfg.getRootNode cfg')
  (cfg' ^. #graph)
  reqNodes
  where
    nextCtxIndex_ = cfg ^. #nextCtxIndex
    outerCtx_ = getCtx cfg'
    mkCfPath = Path nextCtxIndex_ outerCtx_

    (cfg', _) = unfoldGroups cfg

-- | Gets all non-looping paths that reach a return node.
getSimpleReturnPaths
  :: (Ord a, Hashable a)
  => Cfg (CfNode a)
  -> [Path (CfNode a)]
getSimpleReturnPaths cfg = getSimplePathsContaining (G.getTermNodes cfg) cfg

initSequenceChooserState :: [PilNode] -> SequenceChooserState
initSequenceChooserState reqSeq'
  = SequenceChooserState
    (P.SeqAndVisitCounts reqSeq' P.emptyVisitCounts)
    $ UnrollLoopState HashMap.empty HashSet.empty

-- | Generates all paths from a Cfg that visit a sequence of nodes in the correct order.
-- This goes through loops.
-- Once it reaches the last node, it ends the path.
-- Assumes there are no grouping nodes in Cfg.
samplePathContainingSequence_
  :: Monad m
  => m Double
  -> m UUID
  -> StrictDescendantsMap PilNode
  -> SequenceChooserState
  -> PilNode -- start node
  -> Cfg PilNode
  -> m (Either
        (SampleRandomPathError (P.ChooseChildError PilNode))
        (Path PilNode, SequenceChooserState))
samplePathContainingSequence_ randDouble genUUID dmap st startNode cfg = do
  let chooser = choosePathsContainingSequence randDouble genUUID dmap
  runExceptT $ samplePath_ chooser st startNode cfg

-- | Generates all paths from a Cfg that visit a sequence of nodes in the correct order.
-- This goes through loops.
-- Assumes there are no grouping nodes in Cfg.
samplePathContainingSequenceIO
  :: StrictDescendantsMap PilNode
  -> SequenceChooserState
  -> PilNode -- start node
  -> Cfg PilNode
  -> IO (Either
         (SampleRandomPathError (P.ChooseChildError PilNode))
         (Path PilNode, SequenceChooserState))
samplePathContainingSequenceIO = samplePathContainingSequence_ randomIO randomIO

type DefinedVarVersions = HashMap PilVar Word64

data UnrollLoopState = UnrollLoopState
  { definedVars :: DefinedVarVersions
  , visitedNodes :: HashSet PilNode
  } deriving (Eq, Ord, Show, Generic)

-- | Adds defined vars to state. If one exists, it means it has been previously defined,
-- so it is incremented.
addDefinedVars :: PilNode -> DefinedVarVersions -> DefinedVarVersions
addDefinedVars n m = foldr (HashMap.alter f) m . PA.getDefinedVars . Cfg.getNodeData $ n
  where
    f :: Maybe Word64 -> Maybe Word64
    f Nothing = Just 0
    f (Just n') = Just $ n' + 1

updatePilVarName_ :: Word64 -> Symbol -> Symbol
updatePilVarName_ 0 s = s
updatePilVarName_ n s = s <> "_loop" <> show n

-- | If PilVar already exists and is version > 0 then append a `_loopN` to name
updatePilVarName :: DefinedVarVersions -> PilVar -> PilVar
updatePilVarName m pv = case HashMap.lookup pv m of
  Nothing -> pv
  Just 0 -> pv
  Just n -> pv & #symbol %~ updatePilVarName_ n

-- | Updates used vars in statements and adds defined vars to state.
-- This is used on statements that reside in a revisited node in a loop.
updatePossiblyLoopingStmt :: Stmt -> State DefinedVarVersions Stmt
updatePossiblyLoopingStmt stmt@(Pil.Stmt stmtAddr statement) = case statement of
  Pil.Def (Pil.DefOp dest src) -> do
    src' <- substAll src
    dest' <- addDefinedVar dest
    return . mkStmt . Pil.Def $ Pil.DefOp dest' src'

  Pil.DefPhi (Pil.DefPhiOp dest srcs) -> do
    dvv <- get
    let srcs' = flatSubst (updatePilVarName dvv) <$> srcs
    dest' <- addDefinedVar dest
    return . mkStmt . Pil.DefPhi $ Pil.DefPhiOp dest' srcs'
    
  _ -> substAll stmt

  where
    mkStmt = Pil.Stmt stmtAddr
    substAll x = do
      dvv <- get
      return $ recurSubst (updatePilVarName dvv) x

    insertOrIncrement Nothing = Just 0
    insertOrIncrement (Just n') = Just $ n' + 1

    addDefinedVar pv = do
      modify $ HashMap.alter insertOrIncrement pv
      dvv <- get
      return $ updatePilVarName dvv pv
              
updatePossiblyLoopingNode_
  :: Monad m
  => m UUID
  -> PilNode
  -> StateT UnrollLoopState m PilNode
updatePossiblyLoopingNode_ getNewUUID n = do
  dvars <- use #definedVars
  let (stmts', dvars')
        = flip runState dvars
          . traverse updatePossiblyLoopingStmt
          . Cfg.getNodeData
          $ n
  #definedVars .= dvars'
  vnodes <- use #visitedNodes
  n' <- if HashSet.member n vnodes
    then flip Cfg.setNodeUUID n <$> lift getNewUUID
    else do
      #visitedNodes %= HashSet.insert n
      return n
  let n'' = Cfg.setNodeData stmts' n'
  return n''

updatePossiblyLoopingNode
  :: Monad m
  => m UUID
  -> UnrollLoopState
  -> PilNode
  -> m (PilNode, UnrollLoopState)
updatePossiblyLoopingNode getNewUUID st n = runStateT (updatePossiblyLoopingNode_ getNewUUID n) st

data SequenceChooserState = SequenceChooserState
  { seqAndVisitCounts :: P.SeqAndVisitCounts PilNode
  , unrollLoopState :: UnrollLoopState
  } deriving (Eq, Ord, Show, Generic)

-- | This chooser gets a path that hits all nodes along a sequence.
-- It can unroll loops to reach nodes as long as the StrictDescendantsMap was generated on a
-- Cfg that still had backedges.
choosePathsContainingSequence
  :: Monad m
  => m Double
  -> m UUID
  -> StrictDescendantsMap PilNode
  -> ChildChooser (P.ChooseChildError PilNode) SequenceChooserState m BranchType PilNode
choosePathsContainingSequence randDouble genUUID dmap parentNode childHalfEdges = do
  withNestedState #seqAndVisitCounts (P.chooseChildByVisitedDescendantCountAndSequence randDouble dmap parentNode childHalfEdges) >>= \case
    Nothing -> return Nothing
    Just (l, n) -> do
      n' <- withNestedState #unrollLoopState $ updatePossiblyLoopingNode_ (lift genUUID) n
      return
        . Just
        $ P.ChildChoice
          { unmodifiedChoice = (l, n)
          , modifedChoiceForPathInclusion = (l, n')
          }

-- | Expands a call node with another path. Updates ctxIds of inner path,
-- as well as node UUIDs.
-- If inner path does not return, snips off path after call expansion.
-- Returns Nothing if call node not found.
-- WARNING: innerPath must contain nodes with unique node ids!
expandCall
  :: UUID
  -> PilPath
  -> CallNode [Stmt]
  -> PilPath
  -> Maybe PilPath
expandCall leaveFuncUuid outerPath callNode innerPath
  | not (P.hasNode (Cfg.Call callNode) outerPath) = Nothing
  | otherwise = do
      callStmt <- CfgI.getCallStmt callNode
      return $ Path
        { nextCtxIndex = outerPathNextCtxIndex'
        , outerCtx = outerPath ^. #outerCtx
        , path = P.expandNode (Cfg.Call callNode) outerPathPerhapsSnipped (wrapInnerPath callStmt ^. #path)
        }
  where
    outerPathPerhapsSnipped = case retExpr of
      Nothing -> P.removeAfterNode (Cfg.Call callNode) $ outerPath ^. #path
      Just _ -> outerPath ^. #path

    outerPathNextCtxIndex' = outerPath ^. #nextCtxIndex + innerPath ^. #nextCtxIndex
    innerPath' = recurSubst (+ (outerPath ^. #nextCtxIndex)) innerPath
    innerPathCtx' = getCtx innerPath'

    wrapInnerPath :: CallStatement -> PilPath
    wrapInnerPath cstmt = P.append (mkEnterNodePath cstmt) UnconditionalBranch
      . maybe innerPath' (P.append innerPath' UnconditionalBranch)
      $ mkLeaveNodePath cstmt

    mkEnterNodePath :: CallStatement -> PilPath
    mkEnterNodePath = build outerPathNextCtxIndex' . P.start . mkEnterNode
    mkEnterNode :: CallStatement -> CfNode [Stmt]
    mkEnterNode = Cfg.EnterFunc
      . CfgI.mkEnterFuncNode (callNode ^. #uuid) (outerPath ^. #outerCtx) innerPathCtx'

    -- | Return expression of inner path.
    --   Returns Nothing if last stmt of last node is not ret
    retExpr :: Maybe Expression
    retExpr = (lastMay . Cfg.getNodeData $ P.end innerPath') >>= \case
      Pil.Stmt _ (Pil.Ret x) -> return $ x ^. #value
      _ -> Nothing

    mkLeaveNodePath :: CallStatement -> Maybe PilPath
    mkLeaveNodePath = fmap (build outerPathNextCtxIndex' . P.start) . mkLeaveNode
    mkLeaveNode :: CallStatement -> Maybe (CfNode [Stmt])
    mkLeaveNode cstmt = case retExpr of
      Nothing -> Nothing
      Just _ -> return . Cfg.LeaveFunc $ Cfg.LeaveFuncNode
        { prevCtx = innerPathCtx'
        , nextCtx = outerPath ^. #outerCtx
        , callSiteAddress = cstmt ^. #stmt . #addr
        , uuid = leaveFuncUuid
        , nodeData = maybeToList $ C.def' <$> cstmt ^. #resultVar <*> retExpr
        }

expandCallWithNewInnerPathIds :: UUID -> PilPath -> CallNode [Stmt] -> PilPath -> IO (Maybe PilPath)
expandCallWithNewInnerPathIds leaveFuncUuid outerPath callNode innerPath = do
  innerPath' <- safeTraverse giveNewUUID innerPath
  return $ expandCall leaveFuncUuid outerPath callNode innerPath'
  where
    giveNewUUID :: CfNode [Stmt] -> IO (CfNode [Stmt])
    giveNewUUID n = do
      uuid <- randomIO
      return $ setNodeUUID uuid n

makeCfgAcyclic ::
  forall a.
  Hashable a =>
  Cfg (CfNode a) ->
  Cfg (CfNode a)
makeCfgAcyclic cfg = Cfg.removeEdges bedges cfg
  where
    bedges = view #edge <$> Loop.getBackEdges cfg

-- | Samples numSamples random paths from a Cfg.
-- This sampler expects an acyclic Cfg. Otherwise, it might pursue branches
-- that only reach a required node through a loop, which it, at the moment,
-- cannot follow, so the path will lack the required node.
-- It also expects that the Cfg has no groups.
sampleRandomPathsContaining_
  :: (Ord a, Hashable a)
  => StrictDescendantsMap (CfNode a)
  -> HashSet (CfNode a)
  -> Int
  -> Cfg (CfNode a)
  -> IO [Path (CfNode a)]
sampleRandomPathsContaining_ dmap reqSomeNodes numSamples cfg = do
  paths <- replicateM numSamples
    (P.sampleRandomPath
      dmap
      (\ _ _ -> error "should not revisit")
      0
      (Cfg.getRootNode cfg)
      (cfg ^. #graph)
      reqSomeNodes
    )
  return . fmap mkCfPath . nub . mapMaybe hush $ paths
  where
    nextCtxIndex_ = cfg ^. #nextCtxIndex
    outerCtx_ = getCtx cfg
    mkCfPath = Path nextCtxIndex_ outerCtx_

-- | Gets a single random path from a CFG.
sampleRandomPath_
  :: Hashable a
  => ((Int, Int) -> IO Int)
  -> StrictDescendantsMap (CfNode a)
  -> Cfg (CfNode a)
  -> IO (Either (SampleRandomPathError' (CfNode a)) (Path (CfNode a)))
sampleRandomPath_ pickBranch dmap cfg
  = fmap (fmap mkCfPath)
  $ P.sampleRandomPath_
    pickBranch
    dmap
    (\ _ _ -> error "should not revisit")
    0
    (Cfg.getRootNode cfg)
    (cfg ^. #graph)
    HashSet.empty
  where
    nextCtxIndex_ = cfg ^. #nextCtxIndex
    outerCtx_ = getCtx cfg
    mkCfPath = Path nextCtxIndex_ outerCtx_

-- | Gets numSamples random paths from Cfg.
-- This version preps the Cfg by unfolding groups and making it acyclic.
sampleRandomPathsContaining
  :: (Ord a, Hashable a)
  => HashSet (CfNode a)
  -> Int
  -> Cfg (CfNode a)
  -> IO [Path (CfNode a)]
sampleRandomPathsContaining reqSomeNodes numSamples cfg = do
  let dmap = G.calcStrictDescendantsMap cfg''
  sampleRandomPathsContaining_ dmap reqSomeNodes numSamples cfg''
  where
    (cfg', _) = unfoldGroups cfg
    cfg'' = makeCfgAcyclic cfg'

-- | Gets a single random path from a CFG.
sampleRandomPath_'
  :: forall a s e m. (Monad m, Hashable a)
  => ChildChooser e s m BranchType (CfNode a) -- Choose between children
  -> s -- initial chooser state
  -> Cfg (CfNode a)
  -> ExceptT (SampleRandomPathError e) m (Path (CfNode a))
sampleRandomPath_' chooser initState cfg
  = fmap mkCfPath
  $ P.sampleRandomPath_'
    chooser
    initState
    (\ _ _ -> error "should not revisit")
    0
    (Cfg.getRootNode cfg)
    (cfg ^. #graph)
  where
    nextCtxIndex_ = cfg ^. #nextCtxIndex
    outerCtx_ = getCtx cfg
    mkCfPath = Path nextCtxIndex_ outerCtx_

-- | Gets a single random path from a CFG.
samplePath_
  :: forall a s e m. (Monad m, Hashable a)
  => ChildChooser e s m BranchType (CfNode a) -- Choose between children
  -> s -- initial chooser state
  -> CfNode a -- start node
  -> Cfg (CfNode a)
  -> ExceptT (SampleRandomPathError e) m (Path (CfNode a), s)
samplePath_ chooser initState startNode cfg
  = fmap (first mkCfPath)
  $ P.samplePath_
    chooser
    initState
    startNode
    (cfg ^. #graph)
  where
    nextCtxIndex_ = cfg ^. #nextCtxIndex
    outerCtx_ = getCtx cfg
    mkCfPath = Path nextCtxIndex_ outerCtx_
