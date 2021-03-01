module Binja.Function
  ( module Exports
  , convertFunction
  , createFunction
  , getFunctions
  , getLLILFunction
  , getLLILSSAFunction
  , getMLILFunction
  , getMLILSSAFunction
  , getFunctionParameterVariables
  , getFunctionStartingAt
  , getFunctionDataBinaryView
  , hasVariableArguments
  , FromFunction(fromFunction)
  , ToFunction(toFunction)
  ) where

import Binja.Prelude hiding (onException, handle)

import qualified Data.Text as Text


import Binja.C.Helpers ( getFunctionParameterVariables_, functionHasVariableArguments_ )
import qualified Binja.C.Main as BN
import Binja.C.Pointers
import qualified Binja.Types.Function as Func
import Binja.Types.Function as Exports
import qualified Binja.Types.Variable as Var
import Binja.Types.Variable (Variable, BNBoolWithConfidence)
import Binja.Variable (fromBNVariable)

createFunction :: FromFunction fun => BNFunction -> IO fun
createFunction ptr = do
  fn <- Function ptr
        <$> (Text.pack <$> BN.getFunctionName ptr)
        <*> BN.getFunctionStart ptr
  fromFunction fn

convertFunction :: (ToFunction a, FromFunction b)
                => a -> IO b
convertFunction = fromFunction . toFunction


getFunctions :: FromFunction fun => BNBinaryView -> IO [fun]
getFunctions bv = BN.getFunctions bv >>= traverse createFunction

getLLILFunction :: Function -> IO LLILFunction
getLLILFunction fn = LLILFunction
  <$> BN.getFunctionLowLevelIL (fn ^. handle)
  <*> pure fn

getLLILSSAFunction :: Function -> IO LLILSSAFunction
getLLILSSAFunction fn = LLILSSAFunction
  <$> (BN.getFunctionLowLevelIL (fn ^. handle)  >>= BN.getLowLevelILSSAForm)
  <*> pure fn

getMLILFunction :: Function -> IO MLILFunction
getMLILFunction fn = MLILFunction
  <$> BN.getFunctionMediumLevelIL (fn ^. handle)
  <*> pure fn

getMLILSSAFunction :: Function -> IO MLILSSAFunction
getMLILSSAFunction fn = MLILSSAFunction
  <$> (BN.getFunctionMediumLevelIL (fn ^. handle)  >>= BN.getMediumLevelILSSAForm)
  <*> pure fn


class FromFunction fun where
  fromFunction :: Function -> IO fun

class ToFunction fun where
  toFunction :: fun -> Function

instance FromFunction Function where
  fromFunction = pure

instance FromFunction LLILFunction where
  fromFunction = getLLILFunction

instance FromFunction LLILSSAFunction where
  fromFunction = getLLILSSAFunction

instance FromFunction MLILFunction where
  fromFunction = getMLILFunction

instance FromFunction MLILSSAFunction where
  fromFunction = getMLILSSAFunction

--------------

instance ToFunction Function where
  toFunction = identity

instance ToFunction LLILFunction where
  toFunction = view func

instance ToFunction LLILSSAFunction where
  toFunction = view func

instance ToFunction MLILFunction where
  toFunction = view func

instance ToFunction MLILSSAFunction where
  toFunction = view func


getFunctionStartingAt :: FromFunction fun => BNBinaryView -> Maybe BNPlatform -> Address -> IO (Maybe fun)
getFunctionStartingAt bv mplat addr = do
  plat <- maybe (BN.getDefaultPlatform bv) return mplat
  mfn <- BN.getGetAnalysisFunction bv plat addr
  maybe (return Nothing) (fmap Just . createFunction) mfn


getFunctionDataBinaryView :: Function -> IO BNBinaryView
getFunctionDataBinaryView = BN.getFunctionData . view handle

getFunctionParameterVariables :: Function -> IO [Variable]
getFunctionParameterVariables fn = do
  r <- getFunctionParameterVariables_ $ fn ^. Func.handle
  traverse (fromBNVariable fn) $ r ^. Var.vars

hasVariableArguments :: Function -> IO BNBoolWithConfidence
hasVariableArguments fn = do
  functionHasVariableArguments_ $ fn ^. Func.handle
