module Haze.Function
  ( module Exports
  , createCallSite
  , isDirectCall
  ) where

import           Haze.Prelude

import Haze.Types.Function as Exports
import           Hinja.Core                  ( Address
                                             , BNBinaryView
                                             )
import           Hinja.Function              ( Function
                                             , getFunctionStartingAt
                                             )
import qualified Hinja.MLIL          as MLIL
import qualified Data.Set as Set


getDestOp :: CallInstruction -> Maybe (MLIL.Operation (MLIL.Expression F))
getDestOp CallInstruction{_dest=(Just(MLIL.Expression{MLIL._op=op'}))} = Just op'
getDestOp _ = Nothing

isDirectCall :: CallInstruction -> Bool
isDirectCall c = case getDestOp c of
  Just (MLIL.CONST_PTR _) -> True
  _ -> False

createCallSite :: BNBinaryView -> Function -> CallInstruction -> IO CallSite
createCallSite bv func c = CallSite func c <$> case c ^. dest of
  Just dexpr -> case (dexpr ^. MLIL.op :: MLIL.Operation (MLIL.Expression F)) of
    (MLIL.CONST_PTR cpop) -> getFunctionStartingAt bv Nothing addr
                                  >>= return . maybe (DestAddr addr) DestFunc
      where
        addr :: Address
        addr = fromIntegral $ cpop ^. MLIL.constant
    _ -> return $ DestExpr dexpr
  Nothing -> return $ DestColl Set.empty --- probably should be a failure

  
