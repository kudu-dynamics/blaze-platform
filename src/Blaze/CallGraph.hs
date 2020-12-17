module Blaze.CallGraph
  ( module Exports,
    getCallGraph,
    getUndirectedCallGraph
  )
where

import Blaze.Prelude
import Blaze.Types.CallGraph as Exports
import Blaze.Import.CallGraph (CallGraphImporter, getCallSites)
import qualified Blaze.Types.Graph as G
import qualified Streamly.Prelude as S

getCallGraph :: CallGraphImporter a => a -> [Function] -> IO CallGraph
getCallGraph = getCallGraphStreaming

getUndirectedCallGraph :: CallGraph -> CallGraph
getUndirectedCallGraph cg = G.addNodes (toList $ G.nodes cg) . G.fromEdges . fmap ((),) $ edges <> map inverseEdge edges
  where
    edges = snd <$> G.edges cg
    inverseEdge (a,b) = (b,a)

getCallGraphStreaming :: CallGraphImporter a => a -> [Function] -> IO CallGraph
getCallGraphStreaming importer funcs = do
  edges <- S.toList . asyncly $ getCallGraphEdges importer funcs
  let g = G.addNodes funcs . G.fromEdges . fmap ((),) $ edges
  return g

getCallGraphEdges ::
  (StreamingIO t m, CallGraphImporter a) =>
  a ->
  [Function] ->
  t m CallEdge
getCallGraphEdges imp funcs = do
  callee <- S.fromList funcs
  callSite <- liftListIO $ getCallSites imp callee
  S.yield (callSite ^. #caller, callee)
