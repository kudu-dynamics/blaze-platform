module Blaze.Types.Path.FastPath
  ( FastPath (FastPath, path, firstAndLastNode)
  ) where

import qualified Prelude as P
import Blaze.Prelude
import Blaze.Types.Path.AlgaPath ( AlgaPath )
import           Blaze.Graph.Alga                  ( AlgaGraph )
import           Blaze.Types.Path
import           Blaze.Types.Graph                 ( Graph )
import qualified Blaze.Types.Path as Path
import qualified Blaze.Types.Graph as G


data FastPath = FastPath
  { path :: AlgaPath
  -- first and last node are == if path is length 1
  , firstAndLastNode :: Maybe (Node, Node)
  } deriving (Eq, Ord, Show)

-- toList is still really slow...
instance Path FastPath where
  fromList [] = FastPath G.empty Nothing
  fromList [x] = FastPath (G.fromNode x) (Just (x, x))
  fromList (x:xs) = FastPath g $ Just (x, maybe x identity $ lastMay xs)
    where
      g = G.fromEdges . fmap ((),) $ pairs (x:xs)

  -- still slow, and used for equality...
  toList p = case firstAndLastNode p of
    Nothing -> []
    Just (x,_) -> x:(getRest $ Path.succ x g)
      where
        g = path p
        getRest Nothing = []
        getRest (Just y) = y : getRest (Path.succ y g)

  succ node = Path.succ node . path

  pred node = Path.pred node . path

  firstNode = fmap fst . firstAndLastNode

  lastNode = fmap snd . firstAndLastNode

  expandAbstractCall acn ip p = case firstAndLastNode p of
    Nothing -> p -- p is empty, so nothing to expand
    Just (pfnode, plnode) -> FastPath p' $ Just (fstNode, lstNode)
      where
        p' = Path.expandAbstractCall acn (path <$> ip) $ path p
        n = AbstractCall acn
        -- if path's first node is not acn, that will remain first node
        fstNode = bool pfnode (insertableFirstNode ip) $ n == pfnode
        -- if paths's last node is not acn, that will remain last node
        lstNode = bool
          plnode
          (maybe (insertableFirstNode ip) identity
            . Path.lastNode $ insertableFullPath ip)
          (n == plnode)
      

  expandLast ip lac = FastPath p' $ Just (fstNode, lstNode)
    where
      p' = Path.expandLast (path <$> ip) (path <$> lac)
      lacFstNode = maybe (AbstractCall $ Path.lacLastNode lac) fst
                   . firstAndLastNode . Path.lacFullPath $ lac
      --last node of lac is always expanded, so lstNode is last node of ip
      lstNode = maybe (insertableFirstNode ip) snd . firstAndLastNode . insertableFullPath $ ip
      -- if lac's first and last node are the same, it only has one node
      fstNode = bool lacFstNode (insertableFirstNode ip) $ lacFstNode == AbstractCall (Path.lacLastNode lac)
  
  contains n = Path.contains n . path


instance Pretty FastPath where
  pretty = pretty . path
