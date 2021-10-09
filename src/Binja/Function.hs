module Binja.Function (
  module Exports,
  convertFunction,
  createFunction,
  getFunctions,
  getLLILFunction,
  getLLILSSAFunction,
  getMLILFunction,
  getMLILSSAFunction,
  getFunctionParameterVariables,
  getFunctionStartingAt,
  getFunctionData,
  hasVariableArguments,
  FromFunction (fromFunction),
  ToFunction (toFunction),
  isFunctionTooLarge,
  isFunctionAnalysisSkipped,
  getAnalysisSkipReason,
  getFunctionAnalysisSkipOverride,
  setFunctionAnalysisSkipOverride,
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
import qualified Prelude as P
import Binja.C.Util (isNil)
import Binja.C.Enums (BNAnalysisSkipReason, BNFunctionAnalysisSkipOverride)

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
getMLILSSAFunction fn =
  whenM
    (isNil $ fn ^. handle)
    (P.error "Function handle is nil for MLILFunction in getMLILSSAFunction.")
    >> MLILSSAFunction
    <$> (BN.getFunctionMediumLevelIL (fn ^. handle) >>= BN.getMediumLevelILSSAForm)
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


getFunctionData :: Function -> IO BNBinaryView
getFunctionData = BN.getFunctionData_ . view handle

getFunctionParameterVariables :: Function -> IO [Variable]
getFunctionParameterVariables fn = do
  r <- getFunctionParameterVariables_ $ fn ^. Func.handle
  traverse (fromBNVariable fn) $ r ^. Var.vars

hasVariableArguments :: Function -> IO BNBoolWithConfidence
hasVariableArguments = functionHasVariableArguments_ . view handle

isFunctionTooLarge :: Function -> IO Bool
isFunctionTooLarge = BN.isFunctionTooLarge_ . view handle

isFunctionAnalysisSkipped :: Function -> IO Bool
isFunctionAnalysisSkipped = BN.isFunctionAnalysisSkipped_ . view handle

getAnalysisSkipReason :: Function -> IO BNAnalysisSkipReason
getAnalysisSkipReason = BN.getAnalysisSkipReason_ . view handle

getFunctionAnalysisSkipOverride :: Function -> IO BNFunctionAnalysisSkipOverride
getFunctionAnalysisSkipOverride = BN.getFunctionAnalysisSkipOverride_ . view handle

setFunctionAnalysisSkipOverride :: Function -> BNFunctionAnalysisSkipOverride -> IO ()
setFunctionAnalysisSkipOverride = BN.setFunctionAnalysisSkipOverride_ . view handle