module Hinja.Function
  ( module Exports
  , createFunction
  , getFunctions
  , getLLILFunction
  , getLLILSSAFunction
  , getMLILFunction
  , getMLILSSAFunction
  ) where

import Hinja.Prelude hiding (onException, handle)
import qualified Data.Text as Text
import qualified Hinja.C.Main as BN
import Hinja.C.Pointers
import Hinja.Types as Exports ( Function(..)
                              , LLILFunction(..)
                              , LLILSSAFunction(..)
                              , MLILFunction(..)
                              , MLILSSAFunction(..)
                              , Variable(..)
                              , VarType(..)
                              , index
                              , storage
                              , sourceType
                              , varType
                              , handle
                              , name
                              , start
                              , func
                              , confidence
                              , typeClass
                              , width
                              , alignment
                              , signed
                              , signedConfidence
                              , isConst
                              , constConfidence )


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

