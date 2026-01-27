{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -w #-}

module Blaze.CfgDot where


import Blaze.Pretty (Tokenizable, pretty')


import Blaze.Prelude hiding (pred, succ)
import Blaze.Types.Graph (Identifiable)
import Blaze.Types.Graph qualified as G
import Data.GraphViz ( PrintDot(toDot))
import Data.GraphViz.Printing (renderDot)
import qualified Data.GraphViz.Types.Graph as GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Text.Lazy qualified as LazyText
import Data.Text qualified as Text
import Network.URI.Encode (encode)
import Blaze.Types.Cfg (Cfg, CfEdge (..), BranchType (..))
import Data.GraphViz.Types.Graph (addCluster)
import Data.GraphViz.Types (GlobalAttributes(..))
import Blaze.Types.HighCfg (HighCfg(..))
import qualified Data.HashSet as HS
import qualified Blaze.Types.HighCfg as HighCfg
import qualified Debug.Trace as Debug

import System.Process (callCommand)
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (lookupEnv)
import Blaze.Types.HighCfNode (HighCfNode(..), getSubCfgs, getNodeUUID, getNodeData)
import qualified Blaze.Types.Cfg as Cfg

-- | Converts a CFG to a dot graph for visualization.
-- TODO edge to cluster right now if the low edge goes to a node containing a CFG
-- We need compound set to true and then this weird syntax:
--     "source uuid" -> "root node uuid" [lhead="cluster_seq:1f70f3d6-5a2f-42de-9331-f2de0f899c87", color="#228b22", label="True", fontcolor="#2e7d32"];
-- TODO for some reason one of the edges is missing. A low one from a seq to the header in the basic loop case.
toHighCfgDot :: forall a. (Hashable a, Identifiable (HighCfNode a) UUID, Tokenizable a) => HighCfg a -> GraphViz.DotGraph Text
toHighCfgDot cfg = addHighEdges $ addCfgDot Nothing cfg.focus GraphViz.emptyGraph
  where
    addCfgDot :: Maybe GraphViz.GraphID -> Cfg (HighCfNode a) -> GraphViz.DotGraph Text -> GraphViz.DotGraph Text
    addCfgDot parentGraphId subCfg  =
      addDotEdges (Cfg.edges subCfg) .
      addNodes parentGraphId subCfg .
      addSubCfgs parentGraphId subCfg
    -- Add a cluster referencing the parent. Then recursively add sub Cfgs making a new graphId.
    addSubCfgs :: Maybe GraphViz.GraphID -> Cfg (HighCfNode a) -> GraphViz.DotGraph Text -> GraphViz.DotGraph Text
    addSubCfgs parentGraphId cfg' dot = foldr (\(subCfg, parent) ->
                                                 addCfgDot (Just $ graphId parent) subCfg .
                                                 addCluster (graphId parent) parentGraphId [GraphAttrs [Label $ StrLabel $ nodeId parent]]
                                              ) dot $ getSubCfgs cfg'

    addNodes :: Maybe GraphViz.GraphID -> Cfg (HighCfNode a) -> GraphViz.DotGraph Text -> GraphViz.DotGraph Text
    addNodes parentGraphId cfg' dot = foldr (\node dotAcc -> GraphViz.addNode (nodeId node) parentGraphId (nodeAttrs node) dotAcc) dot $ filter (not . isRegion) $ toList $ G.nodes cfg'
    addDotEdges :: [CfEdge (HighCfNode a)] -> GraphViz.DotGraph Text -> GraphViz.DotGraph Text
    addDotEdges edges dot = foldr (\e dotAcc -> GraphViz.addEdge (edgeNodeId e.src) (edgeNodeId e.dst) (edgeAttrs e.branchType False) dotAcc) dot $ filter (\e -> not $ isRegion e.src) edges
    addHighEdges dot = foldr (\e dotAcc -> GraphViz.addEdge (show e.src) (show e.dst) (edgeAttrs e.branchType True) dotAcc) dot $ HS.toList cfg.highEdges
    isRegion (Loop _) = True
    isRegion (Sequence _) = True
    isRegion (WhileDo _ _) = True
    isRegion (ForLoop _) = True
    isRegion _ = False
    nodeAttrs bb@(BasicBlock _) = colorAttr (RGB 173 216 230) <> [Label $ StrLabel $ fromStrict $ Text.replace ", " "\\l" $ pretty' (getNodeData bb) <> "\\l"]
    nodeAttrs bb@(Call _) = colorAttr (RGB 255 182 193) <> [Label $ StrLabel $ fromStrict $ Text.replace ", " "\\l" $ pretty' (getNodeData bb) <> "\\l"]
    nodeAttrs (EnterFunc _) = colorAttr (RGB 221 160 221)
    nodeAttrs (LeaveFunc _) = colorAttr (RGB 255 218 185)
    -- Not used
    nodeAttrs (Loop _) = []
    nodeAttrs (WhileDo _ _) = []
    nodeAttrs (ForLoop _) = []
    nodeAttrs (Sequence _) = []
    highPrefix isHigh orig = if isHigh then "HighEdge" <> (if orig == "" then "" else ":") <> orig else orig
    highEdgeAttr = [ Style [SItem Dashed []] ]
    edgeAttrs bt isHigh = case bt of
      TrueBranch -> [Color [WC (RGB 34 139 34) Nothing], Label $ StrLabel $ highPrefix isHigh "True", FontColor (RGB 46 125 50)] <> if isHigh then highEdgeAttr else []
      FalseBranch -> [Color [WC (RGB 220 20 60) Nothing], Label $ StrLabel $ highPrefix isHigh "False", FontColor (RGB 211 47 47)] <> if isHigh then highEdgeAttr else []
      UnconditionalBranch -> Label (StrLabel $ highPrefix isHigh "") : colorAttr (RGB 119 136 153) <> if isHigh then highEdgeAttr else []
    colorAttr color = [FillColor [WC color Nothing], Style [SItem Filled []]]
    nodeId n = prependNodeId n <> show (getNodeUUID n)
    graphId = GraphViz.Str . LazyText.pack . nodeId
    -- These are regions instead
    prependNodeId (Loop _) = "loop:"
    prependNodeId (WhileDo _ _) = "whiledo:"
    prependNodeId (Sequence _) = "seq:"
    prependNodeId (ForLoop _) = "forloop:"
    prependNodeId _ = ""
    edgeNodeId n = show $ getNodeUUID n

toHighCfgDotLink :: (Hashable a, Tokenizable a) => HighCfg a -> String
toHighCfgDotLink cfg = "https://dreampuf.github.io/GraphvizOnline/?engine=dot#" <> dotTextBase64
  where
    dotText = renderDot $ toDot $ toHighCfgDot cfg
    dotTextBase64 = encode $ LazyText.unpack dotText

toCfgDotLink cfg = toHighCfgDotLink $ HighCfg HS.empty cfg

-- Unsafely print the image to the terminal for debugging
-- Along with a debug statement
-- Note: doing the unsafe perform IO with pure "" to prevent some interleaving
unsafeDebugTraceImage :: (Hashable a, Tokenizable a) => String -> HighCfg a -> b -> b
unsafeDebugTraceImage label cfg = Debug.trace (label <> "\n" <> unsafePerformIO (viewDotCfgInTerm cfg >>= const (pure " ")))

viewDotCfgInTerm :: (Hashable a, Tokenizable a) => HighCfg a -> IO ()
viewDotCfgInTerm cfg = do
  termProgram <- lookupEnv "TERM_PROGRAM"
  term <- lookupEnv "TERM"
  let printTermCommand = case (termProgram, term) of
        (Just "iTerm.app", _) -> Just "iTerm.app"
        (_, Just "xterm-kitty") -> Just "kitty icat"
        _ -> Nothing
  case printTermCommand of
    Nothing -> putStrLn @String "Please use iterm or kitty for this feature.\n Checked $TERM_PROGRAM for iTerm.app and $Term for xterm-kitty."
    Just cmd -> do
        let dotText = renderDot $ toDot $ toHighCfgDot cfg
        withSystemTempFile "cfg.dot" $ \dotPath dotHandle -> do
          hPutStr dotHandle dotText
          hClose dotHandle
          withSystemTempFile "cfg.png" $ \pngPath pngHandle -> do
            hClose pngHandle
            callCommand $ "dot -Tpng " <> dotPath <> " -o " <> pngPath
            callCommand $ cmd <> pngPath
