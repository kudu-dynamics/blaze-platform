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
import Blaze.Types.Function (Function)

import Control.Concurrent.Async (forConcurrently)

getUndirectedCallGraph :: CallGraph -> CallGraph
getUndirectedCallGraph cg = G.addNodes (toList $ G.nodes cg) . G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $ edges <> map inverseEdge edges
  where
    edges = snd <$> (G.toTupleLEdge <$> G.edges cg)
    inverseEdge (a,b) = (b,a)

getCallGraph :: CallGraphImporter a => a -> [Function] -> IO CallGraph
getCallGraph importer funcs = do
  edges <- fmap concat . forConcurrently funcs $ \callee ->
    fmap (\callSite -> (callSite ^. #caller, callee)) <$> getCallSites importer callee
  pure . G.addNodes funcs . G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $ edges
