
module Blaze.Search where

-- import Blaze.Prelude hiding (succ, pred, toList)

-- import Binja.Core (InstructionIndex)
-- import Binja.Function (Function, MLILSSAFunction)
-- import qualified Binja.Function as Func
-- import qualified Blaze.Path as Path
-- import Blaze.Pretty (Pretty, pretty)
-- import qualified Blaze.Types.CallGraph as CG
-- import Blaze.Types.CallGraph (CallGraph, CallSite)
-- import qualified Blaze.Types.Function as BF
-- import qualified Blaze.Types.Graph as G
-- import Blaze.Types.Path (AbstractCallNode, Node, Path)
-- import qualified Data.HashMap.Strict as HMap
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Prelude as P


-- type F = MLILSSAFunction

-- callSiteContainsInstruction :: InstructionIndex F -> CallSite -> Bool
-- callSiteContainsInstruction ix c = ix == c ^. #callInstr . #index

-- nodeContainsInstruction :: InstructionIndex F -> Node -> Bool
-- nodeContainsInstruction ix x = case x of
--   (Path.Condition _) -> False
--   (Path.Ret n) -> checkCallSite n
--   (Path.AbstractPath _) -> False
--   (Path.AbstractCall n) -> checkCallSite n
--   (Path.Call n) -> checkCallSite n
--   (Path.SubBlock n) -> ix < n ^. #end && ix >= n ^. #start
--   where
--     checkCallSite :: HasField' "callSite" a CallSite => a -> Bool
--     checkCallSite n = callSiteContainsInstruction ix $ n ^. #callSite

-- -- | returns first node that contains the instruction
-- firstNodeContainingInstruction :: Path p => InstructionIndex F -> p -> Maybe Node
-- firstNodeContainingInstruction ix = headMay . filter (nodeContainsInstruction ix) . Path.toList


-- -- | returns last node that contains the instruction
-- -- todo: make a toRevList function that is just as fast as toList instead of reverse
-- --      (for Path represented with a Graph)
-- lastNodeContainingInstruction :: Path p => InstructionIndex F -> p -> Maybe Node
-- lastNodeContainingInstruction ix = headMay . filter (nodeContainsInstruction ix) . reverse . Path.toList

-- callSiteCallsFunction :: Function -> CallSite -> Bool
-- callSiteCallsFunction fn c = case c ^. #callDest of
--   (BF.DestAddr addr) -> fn ^. Func.start == addr
--   (BF.DestFunc fn') -> fn == fn'
--   (BF.DestExpr _) -> False -- maybe should check the expr?
--   (BF.DestColl s) -> any g s
--     where
--       g (BF.DestCollAddr addr) = fn ^. Func.start == addr
--       g (BF.DestCollExpr _) = False -- should we check expr?

-- getAbstractCallNodesToFunction :: Path p => Function -> p -> [AbstractCallNode]
-- getAbstractCallNodesToFunction fn = mapMaybe f . Path.toList
--   where
--     f (Path.AbstractCall n) = bool Nothing (Just n) $ callSiteCallsFunction fn $ n ^. #callSite
--     f _ = Nothing

-- -- | Nothing if path doesn't contain instruction
-- snipBeforeInstruction :: (Pretty p, Path p) => InstructionIndex F -> p -> Maybe p
-- snipBeforeInstruction ix p = case dropWhile (not . nodeContainsInstruction ix) $ Path.toList p of
--   [] -> P.error . cs $ pretty p
--     -- Nothing
--   xs -> Just $ Path.fromList xs


-- -- takes, but includes the last one
-- takeWhile' :: (a -> Bool) -> [a] -> [a]
-- takeWhile' _ [] = []
-- takeWhile' p (x:xs)
--   | p x = x : takeWhile' p xs
--   | otherwise = [x]

-- -- | snips path after predicate is true
-- snipAfter_ :: Path p => (Node -> Bool) -> p -> p
-- snipAfter_ pred = Path.fromList . takeWhile' (not . pred) . Path.toList

-- -- | snips path after predicate is true, otherwise returns nothing
-- snipAfter :: Path p => (Node -> Bool) -> p -> Maybe p
-- snipAfter pred p = fmap Path.fromList $ takeWhile'' (not . pred) . Path.toList $ p
--   where
--     takeWhile'' :: Eq a => (a -> Bool) -> [a] -> Maybe [a]
--     takeWhile'' _ [] = Nothing
--     takeWhile'' predicate (x:xs)
--       | predicate x             = (x:) <$> takeWhile'' predicate xs
--       | predicate x && xs == [] = Nothing
--       | otherwise               = Just [x]

-- -- | Nothing if path doesn't contain instruction
-- --   TODO: add funcs for dropping nodes in the Path class
-- snipAfterInstruction :: Path p => InstructionIndex F -> p -> Maybe p
-- snipAfterInstruction ix = snipAfter $ nodeContainsInstruction ix


-- data PathWithCall p = PathWithCall
--   { path :: p
--   , callNode :: AbstractCallNode
--   } deriving (Eq, Ord, Show)

-- instance Pretty p => Pretty (PathWithCall p) where
--   pretty x = "PathWithCall:\n" 
--     <> "callNode: " <> pretty (Path.AbstractCall $ callNode x) <> "\n"
--     <> "path:\n"
--     <> pretty (path x)

-- -- | A path might contain multiple calls to the same function
-- -- these paths are snipped after call
-- pathsWithCallTo :: Path p => Function -> p -> [PathWithCall p]
-- pathsWithCallTo fn p = f <$> getAbstractCallNodesToFunction fn p
--   where
--     f n = PathWithCall (snipAfter_ (== Path.AbstractCall n) p) n

-- -- | Expands AbstractCallNode in first path with second PathWithCall
-- -- result is expanded path, with callNode from second
-- joinPathWithCall :: Path p => PathWithCall p -> PathWithCall p -> PathWithCall p
-- joinPathWithCall pc1 pc2 = case x of
--   Nothing -> P.error "Tried to join PathWithCall that didn't have AbstractCallNode as last"
--   Just (ip, lac) -> PathWithCall
--     { path = Path.expandLast ip lac
--     , callNode = callNode pc2
--     }
--   where
--     x = (,) <$> Path.mkInsertablePath (path pc2)
--             <*> Path.mkLastIsAbstractCall (path pc1)

-- cartesian :: [a] -> [a] -> [[a]]
-- cartesian xs ys = do
--   x <- xs
--   y <- ys
--   return [x, y]

-- -- -- | allCombos [[1 2 3] [4 5] [6]] = [[1 4 6] [1 5 6] [2 4 6] [2 5 6] [3..]]
-- allCombos :: [[a]] -> [[a]]
-- allCombos [] = []
-- allCombos [xs] = do
--   x <- xs
--   return [x]
-- allCombos [xs, ys] = cartesian xs ys
-- allCombos (xs:xss) = do
--   x <- xs
--   ys <- allCombos xss
--   return $ x:ys

-- -- | this is using the inefficient method of searching though all the nodes
-- -- of every path in each function along the call path.
-- searchBetween_ :: forall p. (Path p, Ord p, Pretty p)
--                => HashMap Address CG.Function
--                -> HashMap Address Function
--                -> CallGraph
--                -> Map Function [p]
--                -> Function -> InstructionIndex MLILSSAFunction
--                -> Function -> InstructionIndex MLILSSAFunction
--                -> [p]
-- searchBetween_ cgfuncs bnfuncs cg fpaths fn1 ix1 fn2 ix2
--   | fn1 == fn2 = endPaths
--   | otherwise = results
--   where
--     lookupPair m k = case Map.lookup k m of
--       Nothing -> P.error . cs . pshow
--         $ ( fmap (fmap pretty) callPaths
--           , fmap (bimap pretty pretty) $ Set.toList allCallPairs
--           , k
--           , length startPaths
--           )
--       Just x -> x

--     results = do
--       cp <- callPathsAsPairs
--       pwcCallPath <- allCombos $ (callPairCache `lookupPair`) <$> cp
--       case uncons pwcCallPath of
--         Nothing -> []
--         Just (x, xs) -> do
--           let pwc = foldr (flip joinPathWithCall) x xs
--           end <- endPaths
--           let cond = (,) <$> Path.mkInsertablePath end
--                          <*> Path.mkLastIsAbstractCall (path pwc)
--           case cond of
--             Nothing -> P.error "Tried to join PathWithCall that didn't have AbstractCallNode as last"
--             Just (ip, lac) -> return $ Path.expandLast ip lac

--     -- TODO: shouldn't startPaths and endPaths remove duplicates,
--     -- which might occur after snip removed unique part?
--     startPaths = maybe [] (mapMaybe $ snipBeforeInstruction ix1) . Map.lookup fn1 $ fpaths

--     endPaths = maybe [] (mapMaybe $ snipAfterInstruction ix2) $
--       if fn1 == fn2 then Just startPaths else Map.lookup fn2 fpaths

--     -- we can assume fn1 and fn2 are always at the start and end, and never in the middle
--     -- of the call path (because we are using simplePaths search that has no dups)
--     fpaths' :: Map Function [p]
--     fpaths' = Map.insert fn2 endPaths $
--       if fn1 == fn2 then fpaths else Map.insert fn1 startPaths fpaths

--     -- TODO: The conversion from CG.Function to Function will be removed once
--     --       refactoring of BN types out of Blaze proper is complete.
--     callPaths :: [[Function]]
--     callPaths = fromMaybe [[]] mBnPaths
--       where 
--         fns :: Maybe (CG.Function, CG.Function)
--         fns = (,) <$> HMap.lookup (fn1 ^. Func.start) cgfuncs
--           <*> HMap.lookup (fn2 ^. Func.start) cgfuncs
--         mCgPaths :: Maybe [[CG.Function]]
--         mCgPaths = (\(x, y) -> G.findSimplePaths x y cg) <$> fns
--         mBnPaths :: Maybe [[Function]]
--         mBnPaths = (fmap . mapMaybe) ((`HMap.lookup` bnfuncs) . (^. #address)) <$> mCgPaths
    
--     callPathsAsPairs :: [[(Function, Function)]]
--     callPathsAsPairs = pairs <$> callPaths

--     allCallPairs :: Set (Function, Function)
--     allCallPairs = Set.fromList . join $ callPathsAsPairs

--     callPairCache :: Map (Function, Function) [PathWithCall p]
--     callPairCache = fmap Set.toList . foldr f Map.empty $ do
--       pair@(caller, callee) <- Set.toList allCallPairs
--       path' <- maybe [] identity $ Map.lookup caller fpaths'
--       return (pair, Set.fromList $ pathsWithCallTo callee path')
--       where
--         f (pair, xs) = Map.insertWith (<>) pair xs

