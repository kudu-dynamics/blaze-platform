{-# LANGUAGE RankNTypes #-}

module Blaze.Pil.Analysis where

import qualified Blaze.Pil.Construct as C
import Blaze.Prelude hiding
  ( Symbol,
    group,
    pi,
  )
import Blaze.Types.Pil
  ( AddOp,
    Expression (Expression),
    ExprOp,
    OperationSize,
    PilVar,
    Statement (Def),
    Stmt,
    Symbol,
    FieldAddrOp,
  )
import qualified Prelude as P
import qualified Blaze.Types.Pil as Pil
import qualified Data.List as List
import Blaze.Types.Pil.Analysis
  ( Analysis,
    DefLoadStmt (DefLoadStmt),
    EqMap,
    Index,
    LoadExpr (LoadExpr),
    LoadStmt (LoadStmt),
    MemAddr,
    MemEquivGroup (MemEquivGroup),
    MemStmt
      ( MemDefLoadStmt,
        MemLoadStmt,
        MemStoreStmt
      ),
    MemStorage (MemStorage),
    StoreStmt (StoreStmt),
    runAnalysis,
    symbolGenerator,
  )
import qualified Blaze.Types.Pil.Analysis as A
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import Data.List (nub)
import Data.Sequence
  ( mapWithIndex,
    update,
  )
import qualified Data.Sequence as DSeq
import qualified Data.Set as Set

widthToSize :: Bits -> Pil.OperationSize
widthToSize x = Pil.OperationSize $ toBytes x

sizeToWidth :: Pil.OperationSize -> Bits
sizeToWidth (Pil.OperationSize x) = toBits x

getDefinedVar_ :: Stmt -> Maybe PilVar
getDefinedVar_ (Def d) = Just $ d ^. Pil.var
getDefinedVar_ (Pil.DefPhi d) = Just $ d ^. Pil.dest
getDefinedVar_ _ = Nothing

getDefinedVars :: [Stmt] -> HashSet PilVar
getDefinedVars = HSet.fromList . mapMaybe getDefinedVar_

getVarsFromExpr_ :: Expression -> [PilVar]
getVarsFromExpr_ e = case e ^. Pil.op of
  (Pil.VAR vop) -> [vop ^. Pil.src]
  (Pil.VAR_FIELD x) -> [x ^. Pil.src]
  (Pil.VAR_SPLIT x) -> [x ^. Pil.high, x ^. Pil.low]
  (Pil.UPDATE_VAR x) -> [x ^. Pil.dest]
  x -> concatMap getVarsFromExpr_ x

getVarsFromExpr :: Expression -> HashSet PilVar
getVarsFromExpr = HSet.fromList . getVarsFromExpr_

getDefinedVar :: Stmt -> Maybe PilVar
getDefinedVar (Def d) = Just $ d ^. Pil.var
getDefinedVar _ = Nothing

getVarsFromStmt :: Stmt -> HashSet PilVar
getVarsFromStmt s = foldr f init s
  where
    init = maybe HSet.empty HSet.singleton $ getDefinedVar s
    f x = HSet.union (getVarsFromExpr x)

getRefVars_ :: Stmt -> HashSet PilVar
getRefVars_ = HSet.fromList . concatMap getVarsFromExpr_

-- | Get all vars references in any of the provided statements.
--   NB: Vars that are defined but not used are not considered
--   referenced.
getRefVars :: [Stmt] -> HashSet PilVar
getRefVars = HSet.unions . map getRefVars_

getFreeVars :: [Stmt] -> HashSet PilVar
getFreeVars xs = HSet.difference allVars defined
  where
    defined = getDefinedVars xs
    allVars = getRefVars xs

getAllVars :: [Stmt] -> HashSet PilVar
getAllVars xs = HSet.unions $ fmap ($ xs) [getRefVars, getDefinedVars]

getAllSyms :: [Stmt] -> HashSet Symbol
getAllSyms = HSet.map (^. Pil.symbol) . getAllVars

---- Var -> Var substitution
substVarsInExpr :: (PilVar -> PilVar) -> Expression -> Expression
substVarsInExpr f e = case e ^. Pil.op of
  (Pil.VAR x) -> e & Pil.op .~ Pil.VAR (x & Pil.src %~ f)
  (Pil.VAR_FIELD x) -> e & Pil.op .~ Pil.VAR_FIELD (x & Pil.src %~ f)
  (Pil.VAR_SPLIT x) ->
    e & Pil.op
      .~ Pil.VAR_SPLIT
        ( x & Pil.high %~ f
            & Pil.low %~ f
        )
  _ -> e

-- | ignores left side of Def, Store, and DefPhi
substVars_ :: (PilVar -> PilVar) -> Stmt -> Stmt
substVars_ f (Pil.DefPhi (Pil.DefPhiOp pv vs)) =
  Pil.DefPhi . Pil.DefPhiOp pv . List.nub . fmap f $ vs
substVars_ f s = fmap (substVarsInExpr f) s

substVars :: (PilVar -> PilVar) -> [Stmt] -> [Stmt]
substVars f = fmap $ substVars_ f

---- Var -> Expression substitution
substVarExprInExpr :: (PilVar -> Maybe Expression) -> Expression -> Expression
substVarExprInExpr f x = case x ^. Pil.op of
  (Pil.VAR (Pil.VarOp v)) -> maybe x identity $ f v
  _ -> x & Pil.op %~ fmap (substVarExprInExpr f)

substVarExpr_ :: (PilVar -> Maybe Expression) -> Stmt -> Stmt
substVarExpr_ f = fmap $ substVarExprInExpr f

substVarExpr :: (PilVar -> Maybe Expression) -> [Stmt] -> [Stmt]
substVarExpr f = fmap $ substVarExpr_ f

---- Expression -> Expression substitution
substExprInExpr :: (Expression -> Maybe Expression) -> Expression -> Expression
substExprInExpr f x = maybe x' identity (f x')
  where
    x' = x & Pil.op %~ fmap (substExprInExpr f)

substExprInExprM :: (Expression -> Analysis Expression)
                 -> Expression
                 -> Analysis Expression
substExprInExprM f x = do
  op <- traverse (substExprInExprM f) $ x ^. Pil.op
  f (x & Pil.op .~ op)


substExpr :: (Expression -> Maybe Expression) -> Stmt -> Stmt
substExpr f = fmap $ substExprInExpr f

substExprs :: (Expression -> Maybe Expression) -> [Stmt] -> [Stmt]
substExprs f = fmap (substExpr f)

substExprMap :: ExprMap -> Stmt -> Stmt
substExprMap m = flip (foldr f) $ HMap.toList m
  where
    f :: (Expression, Expression) -> Stmt -> Stmt
    f (a, b) = substExpr (\x -> if x == a then Just b else Nothing)

-----------------



addToEqMap :: (Hashable a, Ord a) => (a, a) -> EqMap a -> EqMap a
addToEqMap (v1, v2) m = case HMap.lookup v2 m of
  Nothing -> HMap.insert v1 v2 m
  Just origin -> HMap.insert v1 origin m

updateVarEqMap :: Stmt -> EqMap PilVar -> EqMap PilVar
updateVarEqMap (Def (Pil.DefOp v1 (Expression _ (Pil.VAR (Pil.VarOp v2))))) m =
  addToEqMap (v1, v2) m
updateVarEqMap _ m = m
 
-- | Each var equivalent to another var is resolved to the
--   earliest defined var. E.g., a = 1, b = a, c = b will
--   result in c mapping to a.
getVarEqMap :: [Stmt] -> EqMap PilVar
getVarEqMap = updateMapsToInOriginVars . foldr updateVarEqMap HMap.empty
  where
    updateMapsToInOriginVars :: EqMap PilVar -> EqMap PilVar
    updateMapsToInOriginVars m = fmap f m
      where
        omap = originsMap m
        omap' = HMap.mapWithKey mergePilVars omap
        f v = case HMap.lookup v omap' of
          Nothing -> v
          (Just v') -> v'

-- | puts VarEqMap in state
putVarEqMap :: [Stmt] -> Analysis ()
putVarEqMap xs = A.varEqMap .= Just (getVarEqMap xs)

originsMap :: (Eq a, Hashable a) => EqMap a -> HashMap a (HashSet a)
originsMap = foldr f HMap.empty . HMap.toList
  where
    f (v1, v2) = HMap.alter g v1
      where
        g Nothing = Just $ HSet.singleton v2
        g (Just s) = Just $ HSet.insert v2 s

-- | Merges the mapsTo of every var in set into the origin var.
mergePilVars :: PilVar -> HashSet PilVar -> PilVar
mergePilVars originVar s = originVar & Pil.mapsTo .~ x
  where
    x = foldr HSet.union HSet.empty y
    y = fmap (view Pil.mapsTo) . HSet.toList $ HSet.insert originVar s

---- Constant Propagation
type VarExprMap = HashMap PilVar Expression

data ConstPropState = ConstPropState
  { _exprMap :: VarExprMap,
    _stmts :: Set Stmt
  }

constantProp :: [Stmt] -> [Stmt]
constantProp xs =
  substVarExpr
    (\v -> HMap.lookup v (_exprMap constPropResult))
    [x | x <- xs, not . Set.member x $ _stmts constPropResult]
  where
    addConst s stmt var cnst =
      ConstPropState
        { _exprMap = HMap.insert var cnst (_exprMap s),
          _stmts = Set.insert stmt (_stmts s)
        }
    constPropResult = foldr f (ConstPropState HMap.empty Set.empty) xs
      where
        f stmt constPropState =
          case stmt of
            (Pil.Def (Pil.DefOp var (Pil.Expression sz (Pil.CONST constOp)))) ->
              addConst constPropState stmt var (Pil.Expression sz (Pil.CONST constOp))
            _ ->
              constPropState

---- Copy Propagation
type ExprMap = HashMap Expression Expression

reduceMap :: (Eq a, Hashable a) => HashMap a a -> HashMap a a
reduceMap m = fmap reduceKey m
  where
    reduceKey v = maybe v reduceKey (HMap.lookup v m)

data CopyPropState = CopyPropState
  { _mapping :: ExprMap,
    _copyStmts :: Set Stmt
  }

_foldCopyPropState :: [Stmt] -> CopyPropState
_foldCopyPropState = foldr f (CopyPropState HMap.empty Set.empty)
  where
    f :: Stmt -> CopyPropState -> CopyPropState
    f stmt copyPropState =
      case stmt of
        (Pil.Def (Pil.DefOp lh_var rh_expr@(Pil.Expression sz (Pil.VAR (Pil.VarOp _))))) ->
          addCopy copyPropState stmt (Pil.Expression sz (Pil.VAR (Pil.VarOp lh_var))) rh_expr
        _ -> copyPropState
    addCopy :: CopyPropState -> Stmt -> Expression -> Expression -> CopyPropState
    addCopy s stmt copy orig =
      CopyPropState
        { _mapping = HMap.insert copy orig (_mapping s),
          _copyStmts = Set.insert stmt (_copyStmts s)
        }

-- | Perform copy propagation on a sequence of statements.
--
--  NB: Copy propagation operates on expressions, while Def statements refer to the lhs as a PilVar.
--      We handle this by removing statements of the form var_a = var_b since all instances of var_a
--      will be replaced with var_b and only the single assignment of var_a to var_b is represented
--      as a PilVar rather than an expression wrapping a PilVar.
copyProp :: [Stmt] -> [Stmt]
copyProp xs =
  substExprs
    (\(e :: Expression) -> HMap.lookup e (_mapping copyPropResult'))
    [x | x <- xs, not . Set.member x $ _copyStmts copyPropResult']
  where
    copyPropResult = _foldCopyPropState xs
    copyPropResult' = copyPropResult {_mapping = reduceMap (_mapping copyPropResult)}
    
isUnusedPhi :: HashSet PilVar -> Stmt -> Bool
isUnusedPhi refs (Pil.DefPhi (Pil.DefPhiOp v _)) = not $ HSet.member v refs
isUnusedPhi _ _ = False


removeUnusedPhi :: [Stmt] -> [Stmt]
removeUnusedPhi stmts = filter (not . isUnusedPhi refs) stmts
  where
    refs = getRefVars stmts


--------------- MEMORY --------------------

usesAddr :: MemAddr -> Stmt -> Bool
usesAddr addr stmt = case stmt of
  Pil.Def Pil.DefOp {_value = Pil.Expression {_op = (Pil.LOAD loadOp)}} ->
    (loadOp ^. Pil.src) == coerce addr
  Pil.Store storeOp ->
    ((storeOp ^. Pil.addr) :: Expression) == coerce addr
  _ -> False

usesStorage :: MemStorage -> Stmt -> Bool
usesStorage storage stmt = case stmt of
  Pil.Def Pil.DefOp {_value = expr@Pil.Expression {_op = (Pil.LOAD loadOp)}} ->
    loadStorage == storage
    where
      loadStorage = mkMemStorage (loadOp ^. Pil.src) (sizeToWidth $ expr ^. Pil.size)
  Pil.Store storeOp ->
    storeStorage == storage
    where
      storeStorage = mkMemStorage (storeOp ^. Pil.addr) (sizeToWidth $ storeOp ^. (Pil.value . Pil.size))
  _ -> False

getAddr :: MemStmt -> MemAddr
getAddr = \case
  MemStoreStmt storeStmt -> storeStmt ^. A.storage . A.start
  MemDefLoadStmt loadStmt -> loadStmt ^. A.loadStmt . A.storage . A.start
  MemLoadStmt x -> x ^. A.storage . A.start

getStorage :: MemStmt -> MemStorage
getStorage = \case
  MemStoreStmt x -> x ^. A.storage
  MemDefLoadStmt x -> x ^. A.loadStmt . A.storage
  MemLoadStmt x -> x ^. A.storage

-- | Helper function for recursively finding loads.
--  NB: this implementation does not recurse on load expressions.
_findLoads :: Expression -> [LoadExpr]
_findLoads e = case e ^. Pil.op of
  Pil.LOAD op -> LoadExpr e : _findLoads (op ^. Pil.src)
  x -> concatMap _findLoads x

-- | Find all loads in a statement.
--  In practice, in BNIL (MLIL SSA), only Def statements contain a Load and Loads are not nested
--  inside of any other expression.
--  I.e., when processing PIL from MLIL SSA we would expect this function to only ever return
--  a list with a single item.
findLoads :: Stmt -> [LoadExpr]
findLoads s = case s of
  (Pil.Def op) -> _findLoads (op ^. Pil.value)
  (Pil.Store op) -> _findLoads (op ^. Pil.value)
  (Pil.Constraint op) -> _findLoads (op ^. Pil.condition)
  _ -> []

memSubst' :: HashMap Word64 Text -> Stmt -> Stmt
memSubst' valMap stmt =
  substExpr (`HMap.lookup` mkMapping valMap stmt) stmt
  where
    traverseToSnd :: (a -> Maybe b) -> a -> Maybe (a, b)
    traverseToSnd g load = (load,) <$> g load
    constLoadsWithText :: HashMap Word64 Text -> Stmt -> [(LoadExpr, Text)]
    constLoadsWithText valMap' x = catMaybes $ traverseToSnd (getConstAddress >=> (`HMap.lookup` valMap')) <$> findLoads x
    -- TODO: Consider using unipatterns
    getConstAddress :: LoadExpr -> Maybe Word64
    getConstAddress l =
      case l of
        LoadExpr Pil.Expression {_op = (Pil.LOAD (Pil.LoadOp Pil.Expression {_op = (Pil.CONST_PTR constPtrOp)}))} ->
          Just $ fromIntegral (constPtrOp ^. Pil.constant)
        _ -> Nothing
    mkMapping :: HashMap Word64 Text -> Stmt -> HashMap Expression Expression
    mkMapping valMap' stmt' = HMap.fromList $ updateWithSize . first coerce <$> constLoadsWithText valMap' stmt'
      where
        updateWithSize :: (Expression, Text) -> (Expression, Expression)
        updateWithSize (load, txt) = (load, C.constStr txt (load ^. Pil.size))

-- | Substitute memory loads of constant pointers
--  to constant values with those constant values.
--  For now, only supporting string values.
memSubst :: HashMap Word64 Text -> [Stmt] -> [Stmt]
memSubst valmap = fmap (memSubst' valmap)

mkStoreStmt :: Index -> Stmt -> Maybe StoreStmt
mkStoreStmt idx s = case s of
  Pil.Store storeOp ->
    Just $ StoreStmt s storeOp storage idx
    where
      storage = mkMemStorage (storeOp ^. Pil.addr) (sizeToWidth $ storeOp ^. (Pil.value . Pil.size))
  _ -> Nothing

-- | A Def Load statement is "def x = [y]" (load is not nested)
mkDefLoadStmt :: Index -> Stmt -> Maybe DefLoadStmt
mkDefLoadStmt idx s = case s of
  Pil.Def (Pil.DefOp pv expr@Pil.Expression {_op = (Pil.LOAD loadOp)}) ->
    Just . DefLoadStmt pv $ LoadStmt s (LoadExpr expr) storage idx
    where
      storage = mkMemStorage (loadOp ^. Pil.src) (sizeToWidth $ expr ^. Pil.size)
  _ -> Nothing

loadExprToLoadStmt :: Index -> Stmt -> LoadExpr -> Maybe LoadStmt
loadExprToLoadStmt idx s x = case x ^. A.expr . Pil.op of
  Pil.LOAD (Pil.LoadOp src') -> Just $ LoadStmt s x mem idx
    where
      mem = MemStorage src' . fromIntegral . (* 8) $ x ^. A.expr . Pil.size
  _ -> Nothing

-- | A statement that is not a DefLoad statement, but has nested loads.
--  Could include a Store stmt.
mkNestedLoadStmts :: Index -> Stmt -> [LoadStmt]
mkNestedLoadStmts idx s = loadExprToLoadStmt idx s `mapMaybe` findLoads s

-- | Creates MemStmts.
-- a statement can have multiple loads and will generate one memstmt for each load
mkMemStmt :: Index -> Stmt -> Maybe [MemStmt]
mkMemStmt idx s = case s of
  Pil.Def Pil.DefOp {_value = Pil.Expression {_op = (Pil.LOAD _)}} ->
    (: []) . MemDefLoadStmt <$> mkDefLoadStmt idx s
  Pil.Store _ -> do
    let nested = MemLoadStmt <$> mkNestedLoadStmts idx s
    ds <- MemStoreStmt <$> mkStoreStmt idx s
    Just $ nested <> [ds]
  _ -> case mkNestedLoadStmts idx s of
    [] -> Nothing
    xs -> Just $ MemLoadStmt <$> xs

-- | Finds memory statements. Update this function if the definition of memory
--  statements changes.
findMemStmts :: [Stmt] -> [MemStmt]
findMemStmts = concat . mapMaybe (uncurry mkMemStmt) . indexed

mkMemEquivGroup :: Maybe StoreStmt -> MemStorage -> [DefLoadStmt] -> [LoadStmt] -> MemEquivGroup
mkMemEquivGroup storeStmt storage defLoadStmts nestedLoadStmts =
  MemEquivGroup
    { _memEquivGroupStore = storeStmt,
      _memEquivGroupStorage = storage,
      _memEquivGroupDefLoads = defLoadStmts,
      _memEquivGroupLoads = nestedLoadStmts
    }

mkMemStorage :: MemAddr -> Bits -> MemStorage
mkMemStorage addr width =
  MemStorage
    { _memStorageStart = addr,
      _memStorageWidth = width
    }

-- | Given a memory address and list of memory statements, build up
--  groups of statements that refer to the same value.
--
--  NB: Assumes all memory statements refer to the given memory address.
findMemEquivGroupsForStorage :: MemStorage -> [MemStmt] -> [MemEquivGroup]
findMemEquivGroupsForStorage storage (x : xs) = reverse groups
  where
    initGroup :: MemEquivGroup
    initGroup = case x of
      MemStoreStmt s ->
        mkMemEquivGroup (Just s) storage [] []
      MemDefLoadStmt s ->
        mkMemEquivGroup Nothing storage [s] []
      MemLoadStmt s ->
        mkMemEquivGroup Nothing storage [] [s]
    groups :: [MemEquivGroup]
    groups = foldl' f [initGroup] xs
    f :: [MemEquivGroup] -> MemStmt -> [MemEquivGroup]
    f gs stmt = case stmt of
      MemStoreStmt s -> g : gs
        where
          g = mkMemEquivGroup (Just s) storage [] []
      MemDefLoadStmt s -> updatedGroup : tailSafe gs
        where
          currGroup = head gs
          updatedGroup = currGroup & A.defLoads %~ (s :)
      MemLoadStmt s -> updatedGroup : tailSafe gs
        where
          currGroup = head gs
          updatedGroup = currGroup & A.loads %~ (s :)
findMemEquivGroupsForStorage _ [] = []

-- | Find equivalent variables that are defined by loading from the same symbolic address
--  and the same memory state—at least for that address.
--  We assume that LOADs only occur in a DEF statement and are not nested.
--  We also assume that STOREs do not include a LOAD expression in the stored value expression.
findMemEquivGroups :: [Stmt] -> [MemEquivGroup]
findMemEquivGroups stmts = groups
  where
    memStmts = findMemStmts stmts
    memStores = nub $ fmap getStorage memStmts
    memStmtsByStorage = do
      memStorage <- memStores
      return $
        do
          memStmt <- memStmts
          guard (memStorage == getStorage memStmt)
          return memStmt
    groups :: [MemEquivGroup]
    groups = concatMap (uncurry findMemEquivGroupsForStorage) $ zip memStores memStmtsByStorage

allMemEquivGroupLoads :: MemEquivGroup -> [LoadStmt]
allMemEquivGroupLoads g =
  (view A.loadStmt <$> g ^. A.defLoads)
    <> g ^. A.loads

-- | Update a sequence of statements to a new sequence of statements where
--  the memory statements referenced by the group have been resolved such that
--  the possible Store statement has been replaced with a Def and all Loads
--  now refer to a new PilVar. If the MemEquivGroup does not include a Store,
--  no Def will be emitted but the Loads will still refer to a common, new PilVar
resolveMemGroup :: MemEquivGroup -> Symbol -> Seq Stmt -> Seq Stmt
resolveMemGroup group name xs =
  case group ^. A.store of
    Nothing ->
      mapWithIndex (updateStmt loadIdxs) xs
    Just storeStmt ->
      mapWithIndex (updateStmt loadIdxs) xs'
      where
        xs' = replaceStore storeStmt name xs
  where
    allLoads :: [LoadStmt]
    allLoads = allMemEquivGroupLoads group
    loadIdxs :: HashSet Index
    loadIdxs = HSet.fromList . fmap (^. A.index) $ allLoads
    varExpr :: Expression
    varExpr = C.var name $ widthToSize (group ^. (A.storage . A.width))
    loadExprMap :: HashMap Expression Expression
    loadExprMap =
      HMap.fromList
        ( zip
            ( map
                (coerce . (^. A.loadExpr))
                allLoads
            )
            (repeat varExpr)
        )
    updateStmt :: HashSet Index -> Index -> Stmt -> Stmt
    updateStmt idxsToUpdate stmtIdx stmt =
      if HSet.member stmtIdx idxsToUpdate
        then substExpr (`HMap.lookup` loadExprMap) stmt
        else stmt

-- | The groups need to be sorted in order by the index of the store statement.
resolveMemGroups :: [MemEquivGroup] -> [Stmt] -> [Stmt]
resolveMemGroups groups stmts = toList result
  where
    stmts' :: Seq Stmt
    stmts' = DSeq.fromList stmts
    names :: [Symbol]
    names = symbolGenerator $ getAllSyms stmts
    result :: Seq Stmt
    result = foldr f stmts' (zip groups names)
    f :: (MemEquivGroup, Symbol) -> Seq Stmt -> Seq Stmt
    f (group, symbol) = resolveMemGroup group symbol

replaceStore :: StoreStmt -> Symbol -> Seq Stmt -> Seq Stmt
replaceStore store symbol = update storeIdx varDef
  where
    storeIdx = store ^. A.index
    storedVal = store ^. (A.op . Pil.value)
    ctx = Nothing -- TODO
    mapsTo = HSet.empty -- TODO
    varDef =
      Pil.Def
        ( Pil.DefOp
            { _var = Pil.PilVar symbol ctx mapsTo,
              _value = storedVal
            }
        )

memoryTransform :: [Stmt] -> [Stmt]
memoryTransform xs =
  resolveMemGroups memGroups xs
  where
    memGroups = findMemEquivGroups xs

data PropInfo = PropInfo
  { substMap :: HashMap Index ExprMap,
    defStmt :: Stmt,
    defAfterIndex :: Maybe Index
  }

propInfoForMemGroup :: MemEquivGroup -> Analysis PropInfo
propInfoForMemGroup mg = do
  s <- A.newSym
  let pv = C.pilVar s
      storageExpr = mg ^. A.storage . A.start
      storageWidth = fromIntegral . (`div` 8) $ mg ^. A.storage . A.width
      loadExpr = C.load storageExpr storageWidth
      defStmt' = Pil.Def (Pil.DefOp pv loadExpr)
      pvExpr = C.var s $ storageExpr ^. Pil.size
  return $
    PropInfo
      { substMap = mkSubstMap pvExpr allLoads,
        defStmt = defStmt',
        defAfterIndex = view A.index <$> mg ^. A.store
      }
  where
    allLoads :: [LoadStmt]
    allLoads = allMemEquivGroupLoads mg
    mkSubstMap :: Expression -> [LoadStmt] -> HashMap Index ExprMap
    mkSubstMap pvExpr = foldr f HMap.empty
      where
        f lstmt = HMap.alter g (lstmt ^. A.index)
          where
            lexpr = lstmt ^. A.loadExpr . A.expr
            g Nothing = Just $ HMap.fromList [(lexpr, pvExpr)]
            g (Just m) = Just $ HMap.insert lexpr pvExpr m

-- | Creates defs for each memgroup load
copyPropMem_ :: [Stmt] -> Analysis [Stmt]
copyPropMem_ xs = do
  propInfos <-
    mapM propInfoForMemGroup
      . filter (not . null . allMemEquivGroupLoads)
      $ memEquivGroups
  let defsAtBeginning :: [Stmt]
      defsAtBeginning =
        mapMaybe
          ( \pi ->
              maybe
                (Just $ defStmt pi)
                (const Nothing)
                $ defAfterIndex pi
          )
          propInfos
      defAfterMap :: HashMap Index (HashSet Stmt)
      defAfterMap =
        foldr (HMap.unionWith (<>)) HMap.empty
          . fmap
            ( \pi ->
                maybe
                  HMap.empty
                  ( flip HMap.singleton
                      . HSet.singleton
                      $ defStmt pi
                  )
                  $ defAfterIndex pi
            )
          $ propInfos
      allSubstsPerIndex :: HashMap Index ExprMap
      allSubstsPerIndex =
        foldr (HMap.unionWith HMap.union) HMap.empty
          . fmap substMap
          $ propInfos
      substAndInsertDefs :: (Index, Stmt) -> [Stmt] -> [Stmt]
      substAndInsertDefs (ix, stmt) ys = newStmt : newDefs <> ys
        where
          newDefs = maybe [] HSet.toList $ HMap.lookup ix defAfterMap
          newStmt =
            maybe stmt (`substExprMap` stmt) $
              HMap.lookup ix allSubstsPerIndex

  return $ defsAtBeginning <> foldr substAndInsertDefs [] (indexed xs)
  where
    memEquivGroups :: [MemEquivGroup]
    memEquivGroups = findMemEquivGroups xs

-- | Copy propagation via memory. Finds and simplifies variables that are copied
--  through symbolic memory addresses that are identified to be equivalent.
--  This is done by constructing store-load chains similar to def-use chains.
copyPropMem :: [Stmt] -> [Stmt]
copyPropMem xs = runAnalysis (copyPropMem_ xs) usedSyms
  where
    usedSyms = HSet.map (^. Pil.symbol) $ getAllVars xs

simplify :: [Stmt] -> [Stmt]
simplify = copyProp . constantProp

simplifyMem :: HashMap Word64 Text -> [Stmt] -> [Stmt]
simplifyMem valMap = memoryTransform . memSubst valMap

subst :: (a -> Maybe a) -> a -> a
subst f x = maybe x identity $ f x

data ParsedAddr = ParsedAddr
  { baseAddrExpr :: Expression
  , fullAddrExpr :: Expression
  } deriving (Eq, Ord, Show)

-- TODO: get rid of this after merge with Kevin's stuff.
type ArrayAddrOp = (FieldAddrOp Expression)
parseArrayAddr :: Expression -> Maybe ParsedAddr
parseArrayAddr _x = Nothing

parseFieldAddr :: Expression -> Maybe ParsedAddr
parseFieldAddr expr =
  case expr ^. Pil.op of
    -- Case where there is a const on the right
    Pil.ADD addOp@(Pil.AddOp _left Pil.Expression {_op = (Pil.CONST _)}) -> do
      baseAddr <- base addOp (^. Pil.left)
      fullAddr <- Pil.Expression (expr ^. Pil.size) . Pil.FIELD_ADDR
                  <$> ( Pil.FieldAddrOp baseAddr <$> offset addOp (^. Pil.right))
      return $ ParsedAddr baseAddr fullAddr
    -- Case where there is a const on the left
    Pil.ADD addOp@(Pil.AddOp Pil.Expression {_op = (Pil.CONST _)} _right) -> do
      baseAddr <- base addOp (^. Pil.right)
      fullAddr <- Pil.Expression (expr ^. Pil.size) . Pil.FIELD_ADDR
                  <$> ( Pil.FieldAddrOp baseAddr <$> offset addOp (^. Pil.left))
      return $ ParsedAddr baseAddr fullAddr
      
    _ -> Nothing
  where
    baseOp :: AddOp Expression -> (AddOp Expression -> Expression) -> Maybe (ExprOp Expression)
    baseOp addOp getExpr =
      Pil.VAR <$> getExpr addOp ^? Pil.op . Pil._VAR
        <|> Pil.CONST_PTR <$> getExpr addOp ^? Pil.op . Pil._CONST_PTR
    baseSize :: AddOp Expression -> (AddOp Expression -> Expression) -> OperationSize
    baseSize addOp getExpr = getExpr addOp ^. Pil.size
    base :: AddOp Expression -> (AddOp Expression -> Expression) -> Maybe Expression
    base addOp getExpr = Pil.Expression (baseSize addOp getExpr) <$> baseOp addOp getExpr
    offset :: AddOp Expression -> (AddOp Expression -> Expression) -> Maybe ByteOffset
    offset addOp getExpr = ByteOffset <$> getExpr addOp ^? Pil.op . Pil._CONST . Pil.constant

parseAddrInLoad :: ( Expression -> Analysis Expression )
                -> Expression
                -> Analysis Expression
parseAddrInLoad addrParser expr =
  case expr ^. Pil.op of
    Pil.LOAD (Pil.LoadOp inner) ->
      outerWrapper <$> addrParser inner
      where
        outerWrapper :: Pil.Expression -> Pil.Expression
        outerWrapper innerExpr =
          Pil.Expression
            (expr ^. Pil.size)
            (Pil.LOAD (Pil.LoadOp innerExpr))
    _ -> return expr

getVarEqMapOrError :: Analysis (EqMap PilVar)
getVarEqMapOrError = maybe (P.error "Must run patVarEqMap first") identity <$>
  use A.varEqMap

substVarEqVarsInExpr :: Expression -> Analysis Expression
substVarEqVarsInExpr x = do
  m <- getVarEqMapOrError
  return $ substVarsInExpr (f m) x
  where
    f m pv = maybe pv identity $ HMap.lookup pv m

substArrayOrFieldAddr :: Expression -> Analysis Expression
substArrayOrFieldAddr x = case parseFieldAddr x of
  Just pa -> do
    baseAddr <- substVarEqVarsInExpr . baseAddrExpr $ pa
    A.fieldBaseAddrs %= HSet.insert baseAddr
    return $ fullAddrExpr pa
    
  Nothing -> case parseArrayAddr x of
    Just pa -> do
      baseAddr <- substVarEqVarsInExpr . baseAddrExpr $ pa
      A.arrayBaseAddrs %= HSet.insert baseAddr
      return $ fullAddrExpr pa
    Nothing -> return x

substAddr_ :: (Expression -> Analysis Expression)
           -> Stmt
           -> Analysis Stmt
substAddr_ addrParser stmt = case stmt of
  Pil.Def (Pil.DefOp var value) ->
    Pil.Def . Pil.DefOp var <$> substExprInExprM parseLoad value
  Pil.Store (Pil.StoreOp addr value) ->
    fmap Pil.Store . Pil.StoreOp
      <$> substExprInExprM addrParser addr
      <*> substExprInExprM parseLoad value
  Pil.Constraint (Pil.ConstraintOp cond) ->
    Pil.Constraint . Pil.ConstraintOp <$> substExprInExprM parseLoad cond
  Pil.Call callOp@(Pil.CallOp (Pil.CallExpr expr) _ _) -> do
    cx <- substExprInExprM parseLoad expr
    return $ Pil.Call (callOp & Pil.dest .~ Pil.CallExpr cx)
  _ -> return stmt
  where
    parseLoad = parseAddrInLoad addrParser

substZeroOffsetFields :: Expression -> Analysis Expression
substZeroOffsetFields expr = case expr ^. Pil.op of
  Pil.FIELD_ADDR _ -> return expr

  -- TODO: doesn't exist yet, but uncomment when it does
  -- Pil.ARRAY_ADDR _ -> return expr

  _ -> do
    x <- substVarEqVarsInExpr expr
    fieldBases <- use A.fieldBaseAddrs
    return $ case HSet.member x fieldBases of
      True ->
        -- Pil.Expression (expr ^. Pil.size) . Pil.FIELD_ADDR
        -- $ Pil.FieldAddrOp expr 0
        expr
      False -> expr


  

substFieldAddr :: Stmt -> Analysis Stmt
substFieldAddr = substAddr_ substArrayOrFieldAddr

substFields :: [Stmt] -> Analysis [Stmt]
substFields = traverse substFieldAddr

substAddrs :: [Stmt] -> [Stmt]
substAddrs stmts = A.runAnalysis_ $ do
  putVarEqMap stmts
  stmts' <- traverse (substAddr_ substArrayOrFieldAddr) stmts

  return stmts'
  --traverse (substAddr_ substZeroOffsetFields) stmts'
  

