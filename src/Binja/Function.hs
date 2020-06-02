module Binja.Function
  ( module Exports
  , createFunction
  , getFunctions
  , getLLILFunction
  , getLLILSSAFunction
  , getMLILFunction
  , getMLILSSAFunction
  , getFunctionStartingAt
  , getFunctionDataBinaryView
  ) where

import Binja.Prelude hiding (onException, handle)

import qualified Data.Text as Text

import qualified Binja.C.Main as BN
import Binja.C.Pointers
import Binja.Types.Function as Exports


createFunction :: BNFunction -> IO Function
createFunction ptr = Function ptr
                     <$> (Text.pack <$> BN.getFunctionName ptr)
                     <*> BN.getFunctionStart ptr

getFunctions :: BNBinaryView -> IO [Function]
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

getFunctionStartingAt :: BNBinaryView -> Maybe BNPlatform -> Address -> IO (Maybe Function)
getFunctionStartingAt bv mplat addr = do
  plat <- maybe (BN.getDefaultPlatform bv) return mplat
  mfn <- BN.getGetAnalysisFunction bv plat addr
  maybe (return Nothing) (fmap Just . createFunction) mfn


getFunctionDataBinaryView :: Function -> IO BNBinaryView
getFunctionDataBinaryView = BN.getFunctionData . view handle
