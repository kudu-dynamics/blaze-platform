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

-- | this is using the inefficent method of searching though all the nodes
-- of every path in each function along the call path.
searchBetween_ :: (Graph () Function g, Path p)
               => BNBinaryView
               -> g
               -> Map Function [p]
               -> Function -> InstructionIndex MLILSSAFunction
               -> Function -> InstructionIndex MLILSSAFunction
               -> IO [p] -- maybe doesn't need IO?
searchBetween_ bv cfg fpaths fn1 ix1 fn2 ix2 = undefined
