module Blaze.IndirectCallResolution where

import Blaze.Prelude

import qualified Blaze.Graph as G
import Binja.Core (BNBinaryView)
import qualified Binja.Function as BnFunc
import qualified Blaze.CallGraph as Cg
import qualified Blaze.Function as Func
import qualified Blaze.Import.CallGraph as Cgi
import qualified Blaze.Import.Source.BinaryNinja.CallGraph as BNCG
import qualified Blaze.Import.Source.BinaryNinja.Pil as BnPil
import Blaze.Import.Source.BinaryNinja.Types (CallInstruction)
import Blaze.Import.Pil as Pili
import Blaze.Import.Source.BinaryNinja (BNImporter)
import Blaze.Types.IndirectCallResolution (IndirectCall, ClassConstructor (ClassConstructor))
import qualified Blaze.Types.IndirectCallResolution as Icr
import qualified Blaze.VTable as VTable
import Data.Tree

getIndirectCallSites :: BNBinaryView -> IO [IndirectCall]
getIndirectCallSites bv = BnFunc.getFunctions bv >>= BnPil.getIndirectCallSites >>= traverse convertToIndirectCall
  where
    convertToIndirectCall :: (BnFunc.Function, CallInstruction) -> IO IndirectCall
    convertToIndirectCall (func, callInstr) = (`Icr.IndirectCall` callInstr) <$> BNCG.convertFunction bv func

getConstructorsInFunction :: BNImporter -> Func.Function -> IO [ClassConstructor]
getConstructorsInFunction imp fn = do
  let bv = imp ^. #binaryView
  vtStores <- Pili.getFuncStatements imp fn 0 >>= VTable.getVTableStores bv
  return $ uncurry (ClassConstructor fn) <$> vtStores

getConstructors :: BNImporter -> IO [ClassConstructor]
getConstructors imp = Cgi.getFunctions imp >>= concatMapM (getConstructorsInFunction imp)

getDirectedCg :: BNImporter -> IO Cg.CallGraph
getDirectedCg imp = Cgi.getFunctions imp >>= Cg.getCallGraph imp

getUndirectedCg :: BNImporter -> IO Cg.CallGraph
getUndirectedCg imp = do
  cg <- Cgi.getFunctions imp >>= Cg.getCallGraph imp
  return $ Cg.getUndirectedCallGraph cg

getEdgeList :: Cg.CallGraph -> [(Func.Function, Func.Function)]
getEdgeList g = G.toTupleEdge . view #edge <$> G.edges g

extractFuncsFromConstructors :: [ClassConstructor] -> [Func.Function]
extractFuncsFromConstructors = map (^. Icr.cFunction)

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
