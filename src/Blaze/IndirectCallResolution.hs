module Blaze.IndirectCallResolution where

import Blaze.Prelude

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Aag
import Binja.Core (BNBinaryView)
import qualified Binja.Function as Func
import qualified Blaze.CallGraph as Cg
import qualified Blaze.Function as Bf
import qualified Blaze.Import.CallGraph as Cgi
import qualified Blaze.Import.Source.BinaryNinja as Bni
import Blaze.Import.Source.BinaryNinja (BNImporter)
import qualified Blaze.Types.Graph.Alga as Ag
import Blaze.Types.IndirectCallResolution (IndirectCall, ClassConstructor (ClassConstructor))
import qualified Blaze.Types.IndirectCallResolution as Icr
import qualified Blaze.VTable as VTable
import Data.Tree

getIndirectCallSites :: BNBinaryView -> IO [IndirectCall]
getIndirectCallSites bv = Func.getFunctions bv >>= Bf.getIndirectCallSites >>= traverse convertToIndirectCall
  where
    convertToIndirectCall :: (Func.Function, Bf.CallInstruction) -> IO IndirectCall
    convertToIndirectCall (func, callInstr) = (`IndirectCall` callInstr) <$> Bni.convertFunction bv func

getConstructorsInFunction :: BNImporter -> Func.Function -> IO [ClassConstructor]
getConstructorsInFunction imp fn = do
  let bv = imp ^. Bni.binaryView
  vtStores <- Bf.getStmtsForFunction imp fn >>= VTable.getVTableStores bv
  cgFn <- Bni.convertFunction bv fn
  return $ uncurry (ClassConstructor cgFn) <$> vtStores

getConstructors :: BNImporter -> IO [ClassConstructor]
getConstructors imp = Cgi.getFunctions imp >>= concatMapM (getConstructorsInFunction bv)

getDirectedCg :: BNImporter -> IO Cg.CallGraph
getDirectedCg imp = Cgi.getFunctions imp >>= Cg.getCallGraph imp

getUndirectedCg :: BNImporter -> IO Cg.CallGraph
getUndirectedCg imp = do
  cg <- Cgi.getFunctions imp >>= Cg.getCallGraph imp
  return $ Cg.getUndirectedCallGraph cg

getEdgeList :: Cg.CallGraph -> [(Cg.Function, Cg.Function)]
getEdgeList = G.edgeList . Ag.adjacencyMap

extractFuncsFromConstructors :: [ClassConstructor] -> [Cg.Function]
extractFuncsFromConstructors = map (^. Icr.cFunction)

getIndirectCallTrees :: BNBinaryView -> IO [Tree Cg.Function]
getIndirectCallTrees bv = do
  cg <- getUndirectedCg importer
  let adMap = Ag.adjacencyMap cg
  iCs <- getIndirectCallSites bv
  let forests = (`Aag.bfsForest` adMap) . (: []) . (^. Icr.iFunction) <$> iCs
  return $ catMaybes $ headMay <$> forests
  where
    importer = Bni.BNImporter bv

pathsToNode :: Eq a => a -> Tree a -> [[a]]
pathsToNode x (Node y ns) = [[x] | x == y] ++ map (y :) (pathsToNode x =<< ns)

getEdgesFromPath :: [(Cg.Function, Cg.Function)] -> Maybe [Cg.Function] -> [(Cg.Function, Cg.Function)]
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

edgesToCandidateConstructors :: [Cg.Function] -> [(Cg.Function, Cg.Function)] -> Tree Cg.Function -> [[(Cg.Function, Cg.Function)]]
edgesToCandidateConstructors constrs edgeL indirectCall = filterOutSameConsecutiveCallees $ map (getEdgesFromPath edgeL . headMay . flip pathsToNode indirectCall) constrs

constructorsForIndirectCalls :: [ClassConstructor] -> [IndirectCall] -> [[[(Cg.Function, Cg.Function)]]] -> [(IndirectCall, [ClassConstructor])]
constructorsForIndirectCalls constrs = zipWith (curry (constrsForIc constrs))
  where
    constrsForIc :: [ClassConstructor] -> (IndirectCall, [[(Cg.Function, Cg.Function)]]) -> (IndirectCall, [ClassConstructor])
    constrsForIc cnstrs (ic, edges) = (ic, ) . fmap fst . filter (\x -> snd x /= []) $ zip cnstrs edges
