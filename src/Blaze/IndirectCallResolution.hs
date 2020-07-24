module Blaze.IndirectCallResolution where

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as Aag
import Binja.Core (BNBinaryView)
import qualified Binja.Function as Func
import qualified Blaze.CallGraph as Cg
import qualified Blaze.Function as Bf
import Blaze.Import.CallGraph as Cgi
import Blaze.Import.Source.BinaryNinja as Bni
import Blaze.Prelude
import qualified Blaze.Types.Graph.Alga as Ag
import qualified Blaze.Types.IndirectCallResolution as Ic
import qualified Blaze.VTable as VTable
import Blaze.Types.Pil (Stmt)
import Blaze.Types.VTable (VTable)
import Data.Tree

getIndirectCallSites :: BNBinaryView -> IO [Ic.IndirectCall]
getIndirectCallSites bv = Func.getFunctions bv >>= Bf.getIndirectCallSites >>= traverse convertToIndirectCall
  where
    convertToIndirectCall :: (Func.Function, Bf.CallInstruction) -> IO Ic.IndirectCall
    convertToIndirectCall (func, callInstr) = (`Ic.IndirectCall` callInstr) <$> Bni.convertFunction bv func

getConstructors :: BNBinaryView -> IO [Ic.ClassConstructor]
getConstructors bv = Func.getFunctions bv >>= concatMapM (getConstructorsInFunction bv)
  where
    getConstructorsInFunction :: BNBinaryView -> Func.Function -> IO [Ic.ClassConstructor]
    getConstructorsInFunction tbv fn = do
      vtStores <- Bf.getStmtsForFunction fn >>= VTable.getVTableStores tbv
      cgFn <- Bni.convertFunction bv fn
      return $ uncurry (Ic.ClassConstructor cgFn) <$> vtStores

getDirectedCg :: BNImporter -> IO Cg.CallGraph
getDirectedCg imp = Cgi.getFunctions imp >>= Cg.getCallGraph imp

getUndirectedCg :: BNImporter -> IO Cg.CallGraph
getUndirectedCg imp = do
  cg <- Cgi.getFunctions imp >>= Cg.getCallGraph imp
  return $ Cg.getUndirectedCallGraph cg

getEdgeList :: Cg.CallGraph -> [(Cg.Function, Cg.Function)]
getEdgeList = G.edgeList . Ag.adjacencyMap

extractFuncsFromConstructors :: [Ic.ClassConstructor] -> [Cg.Function]
extractFuncsFromConstructors = map (^. Ic.cFunction)

getIndirectCallTrees :: BNBinaryView -> IO [Tree Cg.Function]
getIndirectCallTrees bv = do
  cg <- getUndirectedCg importer
  let adMap = Ag.adjacencyMap cg
  iCs <- getIndirectCallSites bv
  let forests = (`Aag.bfsForest` adMap) . (: []) . (^. Ic.iFunction) <$> iCs
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

constructorsForIndirectCalls :: [Ic.ClassConstructor] -> [Ic.IndirectCall] -> [[[(Cg.Function, Cg.Function)]]] -> [(Ic.IndirectCall, [Ic.ClassConstructor])]
constructorsForIndirectCalls constrs = zipWith (curry (constrsForIc constrs))
  where
    constrsForIc :: [Ic.ClassConstructor] -> (Ic.IndirectCall, [[(Cg.Function, Cg.Function)]]) -> (Ic.IndirectCall, [Ic.ClassConstructor])
    constrsForIc cnstrs (ic, edges) = (ic, ) . fmap fst . filter (\x -> snd x /= []) $ zip cnstrs edges