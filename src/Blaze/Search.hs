module Blaze.Search where

import Blaze.Prelude hiding (succ, pred, toList)

import Binja.Function (Function, MLILSSAFunction)
import Binja.Core (InstructionIndex, BNBinaryView)
import qualified Blaze.Path as Path
import Blaze.Types.Path (Path, Node)
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
pathContainsInstruction :: Path p => InstructionIndex F -> p -> Maybe Node
pathContainsInstruction ix = headMay . filter (nodeContainsInstruction ix) . Path.toList


searchBetween_ :: Graph () Function g
               => BNBinaryView
               -> g
               -> Map Function [Path]
               -> Function -> InstructionIndex MLILSSAFunction
               -> Function -> InstructionIndex MLILSSAFunction
               -> IO [Path]
searchBetween_ bv cfg fpaths fn1 ix1 fn2 ix2 = undefined
