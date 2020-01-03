module Blaze.Search where

import Blaze.Prelude hiding (succ, pred, toList)

import Binja.Function (Function, MLILSSAFunction)
import qualified Binja.Function as Func
import Binja.Core (InstructionIndex, BNBinaryView)
import qualified Blaze.Path as Path
import Blaze.Types.Path (Path, Node, AbstractCallNode)
import Blaze.Types.Graph (Graph)
import qualified Blaze.Types.Function as BF
import Blaze.Types.Function (CallSite)
import qualified Blaze.Types.Graph as G
import qualified Data.Map as Map
import qualified Data.Set as Set

type F = MLILSSAFunction

callSiteContainsInstruction :: InstructionIndex F -> CallSite -> Bool
callSiteContainsInstruction ix c = ix == c ^. BF.callInstr . BF.index

nodeContainsInstruction :: InstructionIndex F -> Node -> Bool
nodeContainsInstruction ix x = case x of
  (Path.Condition _) -> False
  (Path.Ret n) -> checkCallSite n
  (Path.AbstractPath _) -> False
  (Path.AbstractCall n) -> checkCallSite n
  (Path.Call n) -> checkCallSite n
  (Path.SubBlock n) -> ix < n ^. Path.end && ix >= n ^. Path.start
  where
    checkCallSite :: Path.HasCallSite a CallSite => a -> Bool
    checkCallSite n = callSiteContainsInstruction ix $ n ^. Path.callSite

-- | returns first node that contains the instruction
firstNodeContainingInstruction :: Path p => InstructionIndex F -> p -> Maybe Node
firstNodeContainingInstruction ix = headMay . filter (nodeContainsInstruction ix) . Path.toList


-- | returns last node that contains the instruction
-- todo: make a toRevList function that is just as fast as toList instead of reverse
--      (for Path represented with a Graph)
lastNodeContainingInstruction :: Path p => InstructionIndex F -> p -> Maybe Node
lastNodeContainingInstruction ix = headMay . filter (nodeContainsInstruction ix) . reverse . Path.toList

callSiteCallsFunction :: Function -> CallSite -> Bool
callSiteCallsFunction fn c = case c ^. BF.callDest of
  (BF.DestAddr addr) -> fn ^. Func.start == addr
  (BF.DestFunc fn') -> fn == fn'
  (BF.DestExpr _) -> False -- maybe should check the expr?
  (BF.DestColl s) -> any g s
    where
      g (BF.DestCollAddr addr) = fn ^. Func.start == addr
      g (BF.DestCollExpr _) = False -- should we check expr?

getAbstractCallNodesToFunction :: Path p => Function -> p -> [AbstractCallNode]
getAbstractCallNodesToFunction fn = mapMaybe f . Path.toList
  where
    f (Path.AbstractCall n) = bool Nothing (Just n) $ callSiteCallsFunction fn $ n ^. Path.callSite
    f _ = Nothing

-- | Nothing if path doesn't contain instruction
snipBeforeInstruction :: Path p => InstructionIndex F -> p -> Maybe p
snipBeforeInstruction ix p = case dropWhile (not . nodeContainsInstruction ix) $ Path.toList p of
  [] -> Nothing
  xs -> Just $ Path.fromList xs


-- takes, but includes the last one
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x = x : takeWhile' p xs
  | otherwise = [x]

-- | snips path after predicate is true
snipAfter_ :: Path p => (Node -> Bool) -> p -> p
snipAfter_ pred = Path.fromList . takeWhile' (not . pred) . Path.toList

snipAfter :: Path p => (Node -> Bool) -> p -> Maybe p
snipAfter pred p = case takeWhile' (not . pred) . Path.toList $ p of
  [] -> Nothing
  xs -> Just . Path.fromList $ xs


-- | Nothing if path doesn't contain instruction
--   TODO: add funcs for dropping nodes in the Path class
snipAfterInstruction :: Path p => InstructionIndex F -> p -> Maybe p
snipAfterInstruction ix = snipAfter $ nodeContainsInstruction ix


data PathWithCall p = PathWithCall
  { path :: p
  , callNode :: AbstractCallNode
  } deriving (Eq, Ord, Show)

-- | A path might contain multiple calls to the same function
-- these paths are snipped after call
pathsWithCallTo :: Path p => Function -> p -> [PathWithCall p]
pathsWithCallTo fn p = f <$> getAbstractCallNodesToFunction fn p
  where
    f n = PathWithCall (snipAfter_ (== (Path.AbstractCall n)) p) n

-- | Expands AbstractCallNode in first path with second PathWithCall
-- result is expanded path, with callNode from second
joinPathWithCall :: Path p => PathWithCall p -> PathWithCall p -> PathWithCall p
joinPathWithCall pc1 pc2 = PathWithCall
  { path = Path.expandAbstractCall (callNode pc1) (path pc2) (path pc1)
  , callNode = callNode pc2 }

-- | this is using the inefficient method of searching though all the nodes
-- of every path in each function along the call path.
searchBetween_ :: forall g p. (Graph () Function g, Path p)
               => BNBinaryView
               -> g
               -> Map Function [p]
               -> Function -> InstructionIndex MLILSSAFunction
               -> Function -> InstructionIndex MLILSSAFunction
               -> [p]
searchBetween_ bv cfg fpaths fn1 ix1 fn2 ix2
  | fn1 == fn2 = endPaths
  | otherwise = do
      cp <- callPathsAsPairs
      undefined
  
  where
    startPaths = maybe [] (mapMaybe $ snipBeforeInstruction ix1) . Map.lookup fn1 $ fpaths
    endPaths = maybe [] (mapMaybe $ snipAfterInstruction ix2) $
      if fn1 == fn2 then Just startPaths else Map.lookup fn1 fpaths

    -- we can assume fn1 and fn2 are always at the start and end, and never in the middle
    -- of the call path (because we are using simplePaths search)
    fpaths' :: Map Function [p]
    fpaths' = Map.insert fn2 endPaths $
      if fn1 == fn2 then fpaths else Map.insert fn1 startPaths fpaths

    callPaths :: [[Function]]
    callPaths = G.findSimplePaths fn1 fn2 cfg
    
    callPathsAsPairs :: [[(Function, Function)]]
    callPathsAsPairs = pairs <$> callPaths

    allCallPairs :: Set (Function, Function)
    allCallPairs = Set.fromList . join $ callPathsAsPairs

    callPairCache :: Map (Function, Function) [PathWithCall p]
    callPairCache = Map.fromList $ do
      pair@(caller, callee) <- Set.toList allCallPairs
      path <- maybe [] identity $ Map.lookup caller fpaths'
      return (pair, pathsWithCallTo callee path)
