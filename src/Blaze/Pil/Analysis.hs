module Blaze.Pil.Analysis where

import Blaze.Prelude hiding (group, Symbol)
import Blaze.Types.Pil
  ( Expression (Expression),
    PilVar,
    Statement (Def),
    Stmt,
    Symbol
  )
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil.Analysis
  ( Index,
    LoadExpr (LoadExpr),
    LoadStmt (LoadStmt),
    MemAddr,
    MemEquivGroup (MemEquivGroup),
    MemStmt (MemLoadStmt, MemStoreStmt),
    MemStorage (MemStorage),
    StoreStmt (StoreStmt),
    Address
  )
import qualified Blaze.Types.Pil.Analysis as A
import qualified Blaze.Pil.Construct as C

import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import Data.List (nub)
import Data.Sequence (update, mapWithIndex)
import qualified Data.Sequence as DSeq
import qualified Data.Set as Set
import qualified Data.Text as Text

widthToSize :: BitWidth -> Pil.OperationSize
widthToSize (BitWidth x) = Pil.OperationSize $ x `div` 8

sizeToWidth :: Pil.OperationSize -> BitWidth
sizeToWidth (Pil.OperationSize x) = BitWidth $ x * 8

getDefinedVars_ :: Stmt -> [PilVar]
getDefinedVars_ (Def d) = [d ^. Pil.var]
getDefinedVars_ _ = []

getDefinedVars :: [Stmt] -> HashSet PilVar
getDefinedVars = HSet.fromList . concatMap getDefinedVars_

getVarsFromExpr_ :: Expression -> [PilVar]
getVarsFromExpr_ e = case e ^. Pil.op of
  (Pil.VAR vop) -> [vop ^. Pil.src]
  (Pil.VAR_FIELD x) -> [x ^. Pil.src]
  (Pil.VAR_ALIASED x) -> [x ^. Pil.src]
  (Pil.VAR_ALIASED_FIELD x) -> [x ^. Pil.src]
  (Pil.VAR_PHI x) -> x ^. Pil.dest : x ^. Pil.src
  (Pil.VAR_SPLIT x) -> [x ^. Pil.high, x ^. Pil.low]
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

-- |Get all vars references in any of the provided statements.
--  NB: Vars that are defined but not used are not considered
--  referenced.
getRefVars :: [Stmt] -> HashSet PilVar
getRefVars = HSet.unions . map getRefVars_

getFreeVars :: [Stmt] -> HashSet PilVar
getFreeVars xs = HSet.difference allVars defined
  where
    defined = getDefinedVars xs
    allVars = getRefVars xs

getAllVars :: [Stmt] -> HashSet PilVar
getAllVars xs = HSet.unions $ fmap ($ xs) [getRefVars, getDefinedVars]

---- Var -> Var substitution
substVarsInExpr :: (PilVar -> PilVar) -> Expression -> Expression
substVarsInExpr f e = case e ^. Pil.op of
  (Pil.VAR x) -> e & Pil.op .~ Pil.VAR (x & Pil.src %~ f)
  (Pil.VAR_FIELD x) -> e & Pil.op .~ Pil.VAR_FIELD (x & Pil.src %~ f)
  (Pil.VAR_ALIASED x) -> e & Pil.op .~ Pil.VAR_ALIASED (x & Pil.src %~ f)
  (Pil.VAR_ALIASED_FIELD x) -> e & Pil.op .~ Pil.VAR_ALIASED_FIELD (x & Pil.src %~ f)
  (Pil.VAR_PHI x) -> e & Pil.op .~ Pil.VAR_PHI (x & Pil.src %~ fmap f
                                                    & Pil.dest %~ f)
  (Pil.VAR_SPLIT x) -> e & Pil.op .~ Pil.VAR_SPLIT (x & Pil.high %~ f
                                                        & Pil.low %~ f)
  _ -> e

substVars_ :: (PilVar -> PilVar) -> Stmt -> Stmt
substVars_ f = fmap $ substVarsInExpr f

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
substExprInExpr f x = recurse $ maybe x identity (f x)
  where
    recurse e = e & Pil.op %~ fmap (substExprInExpr f)

substExpr :: (Expression -> Maybe Expression) -> Stmt -> Stmt
substExpr f = fmap $ substExprInExpr f

substExprs :: (Expression -> Maybe Expression) -> [Stmt] -> [Stmt]
substExprs f = fmap (substExpr f)

-----------------

type EqMap a = HashMap a a


addToEqMap :: (Hashable a, Ord a) => (a, a) -> EqMap a -> EqMap a
addToEqMap (v1, v2) m = case HMap.lookup v2 m of
  Nothing -> HMap.insert v1 v2 m
  Just origin -> HMap.insert v1 origin m

updateVarEqMap :: Stmt -> EqMap PilVar -> EqMap PilVar
updateVarEqMap (Def (Pil.DefOp v1 (Expression _ (Pil.VAR (Pil.VarOp v2))))) m
  = addToEqMap (v1, v2) m
updateVarEqMap _ m = m

-- |Each var equivalent to another var is resolved to the 
--  earliest defined var. E.g., a = 1, b = a, c = b will
--  result in c mapping to a.
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

originsMap :: EqMap PilVar -> HashMap PilVar (HashSet PilVar)
originsMap = foldr f HMap.empty . HMap.toList
  where
    f (v1, v2) = HMap.alter g v1 where
      g Nothing = Just $ HSet.singleton v2
      g (Just s) = Just $ HSet.insert v2 s

-- |Merges the mapsTo of every var in set into the origin var.
mergePilVars :: PilVar -> HashSet PilVar -> PilVar
mergePilVars originVar s = originVar & Pil.mapsTo .~ x where
  x = foldr HSet.union HSet.empty y
  y = fmap (view Pil.mapsTo) . HSet.toList $ HSet.insert originVar s

---- Constant Propagation
type VarExprMap = HashMap PilVar Expression

data ConstPropState
  = ConstPropState
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

data CopyPropState
  = CopyPropState
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

-- |Perform copy propagation on a sequence of statements.
--
-- NB: Copy propagation operates on expressions, while Def statements refer to the lhs as a PilVar.
--     We handle this by removing statements of the form var_a = var_b since all instances of var_a
--     will be replaced with var_b and only the single assignment of var_a to var_b is represented
--     as a PilVar rather than an expression wrapping a PilVar.
copyProp :: [Stmt] -> [Stmt]
copyProp xs =
  substExprs
    (\(e :: Expression) -> HMap.lookup e (_mapping copyPropResult'))
    [x | x <- xs, not . Set.member x $ _copyStmts copyPropResult']
  where
    copyPropResult = _foldCopyPropState xs
    copyPropResult' = copyPropResult {_mapping = reduceMap (_mapping copyPropResult)}
                            
usesAddr :: MemAddr -> Stmt -> Bool
usesAddr addr stmt = case stmt of
  Pil.Def Pil.DefOp {_value = Pil.Expression {_op = (Pil.LOAD loadOp)}} 
    -> (loadOp ^. Pil.src) == coerce addr
  Pil.Store storeOp 
    -> ((storeOp ^. Pil.addr) :: Expression) == coerce addr
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
getAddr memStmt = case memStmt of
  MemStoreStmt storeStmt -> storeStmt ^. (A.storage . A.start)
  MemLoadStmt loadStmt -> loadStmt ^. (A.storage . A.start)

getStorage :: MemStmt -> MemStorage
getStorage memStmt = case memStmt of
  MemStoreStmt storeStmt -> storeStmt ^. A.storage
  MemLoadStmt loadStmt -> loadStmt ^. A.storage

-- |Helper function for recursively finding loads. 
-- NB: this implementation does not recurse on load expressions.
_findLoads :: Expression -> [LoadExpr]
_findLoads e = case e ^. Pil.op of
  Pil.LOAD op -> LoadExpr e : _findLoads (op ^. Pil.src)
  x -> concatMap _findLoads x

-- |Find all loads in a statement. 
-- In practice, in BNIL (MLIL SSA), only Def statements contain a Load and Loads are not nested
-- inside of any other expression.
-- I.e., when processing PIL from MLIL SSA we would expect this function to only ever return
-- a list with a single item.
findLoads :: Stmt -> [LoadExpr]
findLoads s = case s of
  (Pil.Def op) -> _findLoads (op ^. Pil.value)
  (Pil.Store op) -> _findLoads (op ^. Pil.value)
  (Pil.Constraint op) -> _findLoads (op ^. Pil.condition)
  _ -> []

memSubst' :: HashMap Address Text -> Stmt -> Stmt
memSubst' valMap stmt =
  substExpr (`HMap.lookup` mkMapping valMap stmt) stmt
  where
    traverseToSnd :: (a -> Maybe b) -> a -> Maybe (a, b)
    traverseToSnd g load = (load,) <$> g load
    constLoadsWithText :: HashMap Address Text -> Stmt -> [(LoadExpr, Text)]
    constLoadsWithText valMap' x = catMaybes $ traverseToSnd (getConstAddress >=> (`HMap.lookup` valMap')) <$> findLoads x
    -- TODO: Consider using unipatterns
    getConstAddress :: LoadExpr -> Maybe Address
    getConstAddress l =
      case l of
        LoadExpr Pil.Expression {_op = (Pil.LOAD (Pil.LoadOp Pil.Expression {_op = (Pil.CONST_PTR constPtrOp)}))} ->
          Just $ fromIntegral (constPtrOp ^. Pil.constant)
        _ -> Nothing
    mkMapping :: HashMap Address Text -> Stmt -> HashMap Expression Expression
    mkMapping valMap' stmt' = HMap.fromList $ updateWithSize . first coerce <$> constLoadsWithText valMap' stmt'
      where
        updateWithSize :: (Expression, Text) -> (Expression, Expression)
        updateWithSize (load, txt) = (load, C.constStr txt (load ^. Pil.size)) 

-- |Substitute memory loads of constant pointers
-- to constant values with those constant values.
-- For now, only supporting string values.
memSubst :: HashMap Address Text -> [Stmt] -> [Stmt]
memSubst valmap = fmap (memSubst' valmap)

mkStoreStmt :: Index -> Stmt -> Maybe StoreStmt
mkStoreStmt idx s = case s of
  Pil.Store storeOp ->
    Just $ StoreStmt s storeOp storage idx
      where
        storage = mkMemStorage (storeOp ^. Pil.addr) (sizeToWidth $ storeOp ^. (Pil.value . Pil.size))
  _ -> Nothing

mkLoadStmt :: Index -> Stmt -> Maybe LoadStmt
mkLoadStmt idx s = case s of
  Pil.Def Pil.DefOp {_value = expr@Pil.Expression {_op = (Pil.LOAD loadOp)}} ->
    Just $ LoadStmt s (LoadExpr expr) storage idx
      where
        storage = mkMemStorage (loadOp ^. Pil.src) (sizeToWidth $ expr ^. Pil.size)
  _ -> Nothing

-- Create a MemStmt
mkMemStmt :: Index -> Stmt -> Maybe MemStmt
mkMemStmt idx s = case s of
  Pil.Def Pil.DefOp {_value = Pil.Expression {_op = (Pil.LOAD _)}} ->
    MemLoadStmt <$> mkLoadStmt idx s
  Pil.Store _ -> MemStoreStmt <$> mkStoreStmt idx s
  _ -> Nothing

-- |Finds memory statements. Update this function if the definition of memory
-- statements changes.
findMemStmts :: [Stmt] -> [MemStmt]
findMemStmts = mapMaybe (uncurry mkMemStmt) . indexed

mkMemEquivGroup :: Maybe StoreStmt -> MemStorage -> [LoadStmt] -> MemEquivGroup
mkMemEquivGroup storeStmt storage loadStmts = 
  MemEquivGroup
    { _memEquivGroupStore = storeStmt,
      _memEquivGroupStorage = storage,
      _memEquivGroupLoads = loadStmts}

mkMemStorage :: MemAddr -> BitWidth -> MemStorage
mkMemStorage addr width = 
  MemStorage 
    { _memStorageStart = addr,
      _memStorageWidth = width}

-- |Given a memory address and list of memory statements, build up
-- groups of statements that refer to the same value.
--
-- NB: Assumes all memory statements refer to the given memory address.
findMemEquivGroupsForStorage :: MemStorage -> [MemStmt] -> [MemEquivGroup]
findMemEquivGroupsForStorage storage (x:xs) = reverse groups
  where
    initGroup = case x of
      MemStoreStmt s ->
        mkMemEquivGroup (Just s) storage []
      MemLoadStmt s ->
        mkMemEquivGroup Nothing storage [s]
    groups = foldl' f [initGroup] xs
    f gs stmt = case stmt of
      MemStoreStmt storeStmt -> g : gs
        where
          g = mkMemEquivGroup (Just storeStmt) storage []
      MemLoadStmt loadStmt -> updatedGroup : tailSafe gs
        where
          currGroup = head gs
          updatedGroup = currGroup & A.loads %~ (loadStmt:)
findMemEquivGroupsForStorage _ [] = []

-- |Find equivalent variables that are defined by loading from the same symbolic address
-- and the same memory state—at least for that address.
-- We assume that LOADs only occur in a DEF statement and are not nested.
-- We also assume that STOREs do not include a LOAD expression in the stored value expression.
findMemEquivGroups :: [Stmt] -> [MemEquivGroup]
findMemEquivGroups stmts = groups
  where
    memStmts = findMemStmts stmts
    memStores = nub $ fmap getStorage memStmts
    memStmtsByStorage = do memStorage <- memStores
                           return $
                              do memStmt <- memStmts
                                 guard (memStorage == getStorage memStmt)
                                 return memStmt
    groups :: [MemEquivGroup]
    groups = concatMap (uncurry findMemEquivGroupsForStorage) $ zip memStores memStmtsByStorage

-- |Update a sequence of statements to a new sequence of statements where
-- the memory statements referenced by the group have been resolved such that
-- the possible Store statement has been replaced with a Def and all Loads
-- now refer to a new PilVar. If the MemEquivGroup does not include a Store,
-- no Def will be emitted but the Loads will still refer to a common, new PilVar
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
    loadIdxs :: HashSet Index
    loadIdxs = HSet.fromList . fmap (^. A.index) . (^. A.loads) $ group
    varExpr :: Expression
    varExpr = C.var name $ widthToSize (group ^. (A.storage . A.width))
    loadExprMap :: HashMap Expression Expression
    loadExprMap = HMap.fromList (zip (map (coerce . (^. A.loadExpr))
                                          (group ^. A.loads)) 
                                     (repeat varExpr))
    updateStmt :: HashSet Index -> Index -> Stmt -> Stmt
    updateStmt idxsToUpdate stmtIdx stmt = 
      if HSet.member stmtIdx idxsToUpdate then
        substExpr (`HMap.lookup` loadExprMap) stmt
      else 
        stmt

-- | The groups need to be sorted in order by the index of the store statement.
resolveMemGroups :: [MemEquivGroup] -> [Stmt] -> [Stmt]
resolveMemGroups groups stmts = toList result
  where
    stmts' :: Seq Stmt
    stmts' = DSeq.fromList stmts

    names :: [Symbol]
    names = symbolGenerator (HSet.map (^. Pil.symbol) $ getAllVars stmts)

    result :: Seq Stmt
    result = foldr f stmts' (zip groups names)

    f :: (MemEquivGroup, Symbol) -> Seq Stmt -> Seq Stmt
    f (group, symbol) = resolveMemGroup group symbol

-- TODO: Make this better.
-- |Generate variable names.
symbolGenerator :: HashSet Symbol -> [Symbol]
symbolGenerator usedNames = [x | x <- names, not $ HSet.member x usedNames]
  where
    letters :: String
    letters = ['a'..'z']

    names :: [Symbol]
    names = [Text.pack [a, b, c] | a <- letters, 
                                   b <- letters,
                                   c <- letters]

replaceStore :: StoreStmt -> Symbol -> Seq Stmt -> Seq Stmt
replaceStore store symbol = update storeIdx varDef
  where
    storeIdx = store ^. A.index
    storedVal = store ^. (A.op . Pil.value)
    func = Nothing -- TODO
    ctxIndex = Nothing -- TODO
    mapsTo = HSet.empty -- TODO
    varDef = Pil.Def (Pil.DefOp
                      { _var = Pil.PilVar symbol func ctxIndex mapsTo,
                        _value = storedVal})

memoryTransform :: [Stmt] -> [Stmt]
memoryTransform xs =
  resolveMemGroups memGroups xs
    where
      memGroups = findMemEquivGroups xs

-- |Copy propagation via memory. Finds and simplifies variables that are copied
-- through symbolic memory addresses that are identified to be equivalent.
-- This is done by constructing store-load chains similar to def-use chains.
copyPropMem :: [Stmt] -> [Stmt]
copyPropMem xs = substExprs (\v -> HMap.lookup v (_mapping propResult)) xs
  where
    _memEquivGroups = findMemEquivGroups xs
    propResult = foldr f (CopyPropState HMap.empty Set.empty) xs
      where
        f _stmt propState =
          propState

simplify :: [Stmt] -> [Stmt]
simplify = copyProp . constantProp
