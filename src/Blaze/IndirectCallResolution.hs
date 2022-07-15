module Blaze.IndirectCallResolution where

import Blaze.Prelude

-- import Binja.Core (BNBinaryView)
-- import Binja.Function qualified as BnFunc
import Blaze.CallGraph qualified as Cg
import Blaze.Function qualified as Func
import Blaze.Graph qualified as G
import Blaze.Import.CallGraph (CallGraphImporter)
import Blaze.Import.CallGraph qualified as Cgi
-- import Blaze.Import.Pil as Pili
-- import Blaze.Import.Source.BinaryNinja (BNImporter)
-- import Blaze.Import.Source.BinaryNinja.CallGraph qualified as BNCG
-- import Blaze.Import.Source.BinaryNinja.Pil qualified as BnPil
-- import Blaze.Import.Source.BinaryNinja.Types (CallInstruction)
import Blaze.Types.IndirectCallResolution (ClassConstructor, IndirectCall)
-- import Blaze.Types.IndirectCallResolution qualified as Icr
-- import Blaze.VTable qualified as VTable
import Data.Tree

---- TODO: This code isn't currently used. Needs to be updated.

-- TODO: We will need a CallGraphImporter to fetch the functions, but a PilImporter to find indirect calls
-- getIndirectCallSites :: (CallGraphImporter a, PilImporter a) => a -> IO [IndirectCall]
-- getIndirectCallSites imp = do
--   callSites <- Cgi.getFunctions imp >>= Cgi.getCallSites
--   where
--     convertToIndirectCall :: (BnFunc.Function, CallInstruction) -> IO IndirectCall
--     convertToIndirectCall (func, callInstr) = (`Icr.IndirectCall` callInstr) <$> BNCG.convertFunction bv func

-- getConstructorsInFunction :: PilImporter a => a -> Func.Function -> IO [ClassConstructor]
-- getConstructorsInFunction imp fn = do
--   let bv = imp ^. #binaryView
--   vtStores <- Pili.getFuncStatements imp fn 0 >>= VTable.getVTableStores bv
--   return $ uncurry (ClassConstructor fn) <$> vtStores

-- getConstructors :: CallGraphImporter a => a -> IO [ClassConstructor]
-- getConstructors imp = Cgi.getFunctions imp >>= concatMapM (getConstructorsInFunction imp)

getDirectedCg :: CallGraphImporter a => a -> IO Cg.CallGraph
getDirectedCg imp = Cgi.getFunctions imp >>= Cg.getCallGraph imp

getUndirectedCg :: CallGraphImporter a => a -> IO Cg.CallGraph
getUndirectedCg imp = do
  cg <- Cgi.getFunctions imp >>= Cg.getCallGraph imp
  return $ Cg.getUndirectedCallGraph cg

getEdgeList :: Cg.CallGraph -> [(Func.Function, Func.Function)]
getEdgeList g = G.toTupleEdge . view #edge <$> G.edges g

extractFuncsFromConstructors :: [ClassConstructor] -> [Func.Function]
extractFuncsFromConstructors = map (view #function)

-- TODO: Rewrite using bfs method from Graph type class.
-- getIndirectCallTrees :: BNBinaryView -> IO [Tree Func.Function]
-- getIndirectCallTrees bv = do
--   cg <- getUndirectedCg importer
--   let adMap = Ag.adjacencyMap cg
--   iCs <- getIndirectCallSites bv
--   let forests = (`Aag.bfsForest` adMap) . (: []) . (^. Icr.iFunction) <$> iCs
--   return $ catMaybes $ headMay <$> forests
--   where
--     importer = Bni.BNImporter bv

pathsToNode :: Eq a => a -> Tree a -> [[a]]
pathsToNode x (Node y ns) = [[x] | x == y] ++ map (y :) (pathsToNode x =<< ns)

getEdgesFromPath :: [(Func.Function, Func.Function)] -> Maybe [Func.Function] -> [(Func.Function, Func.Function)]
getEdgesFromPath _ Nothing = []
getEdgesFromPath _ (Just []) = []
getEdgesFromPath _ (Just [_]) = []
getEdgesFromPath edgeL (Just (n : m : ns)) = getEdge edgeL n m <> getEdgesFromPath edgeL (Just (m : ns))
  where
    getEdge el n0 n1 = filter (\x -> (fst x == n0 || snd x == n0) && (fst x == n1 || snd x == n1)) el

-- | ensures we don't have a path from a caller to a caller of a function
filterOutSameConsecutiveCallees :: Eq a => [[(a, a)]] -> [[(a, a)]]
filterOutSameConsecutiveCallees = filter (not . containsSameY)
  where
    containsSameY [] = False
    containsSameY [_] = False
    containsSameY (n : m : ns) = sameY n m || containsSameY (m : ns)
    sameY n0 n1 = snd n0 == snd n1

edgesToCandidateConstructors :: [Func.Function] -> [(Func.Function, Func.Function)] -> Tree Func.Function -> [[(Func.Function, Func.Function)]]
edgesToCandidateConstructors constrs edgeL indirectCall = filterOutSameConsecutiveCallees $ map (getEdgesFromPath edgeL . headMay . flip pathsToNode indirectCall) constrs

constructorsForIndirectCalls :: [ClassConstructor] -> [IndirectCall] -> [[[(Func.Function, Func.Function)]]] -> [(IndirectCall, [ClassConstructor])]
constructorsForIndirectCalls constrs = zipWith (curry (constrsForIc constrs))
  where
    constrsForIc :: [ClassConstructor] -> (IndirectCall, [[(Func.Function, Func.Function)]]) -> (IndirectCall, [ClassConstructor])
    constrsForIc cnstrs (ic, edges) = (ic, ) . fmap fst . filter (\x -> snd x /= []) $ zip cnstrs edges
