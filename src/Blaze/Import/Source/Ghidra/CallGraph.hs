module Blaze.Import.Source.Ghidra.CallGraph where


import Blaze.Prelude hiding (Symbol)

import qualified Blaze.Types.Function as Func
import Blaze.Import.Source.Ghidra.Types (convertAddress)
import Blaze.Types.CallGraph (
  CallSite (
    CallSite
  ),
 )
import qualified Blaze.Types.CallGraph as CG
import Blaze.Types.Function (
  Function,
  FuncParamInfo (FuncParamInfo, FuncVarArgInfo),
  ParamInfo (ParamInfo),
 )
import qualified Blaze.Types.Function as BFunc

import Ghidra.Core (runGhidra)
import Ghidra.State (GhidraState)
import qualified Ghidra.State as State
import qualified Ghidra.Function as G
import qualified Ghidra.Types.Function as G
import qualified Ghidra.Types as J
import qualified Ghidra.Reference as GRef


getFuncAddr :: G.Function -> Address
getFuncAddr = convertAddress . view #startAddress

convertParam :: G.Parameter -> ParamInfo
convertParam p = ParamInfo (p ^. #name) BFunc.Unknown

toGhidraFunction :: GhidraState -> Function -> IO J.Function
toGhidraFunction gs fn = getFunction_ gs (fn ^. #address) >>= \case
    Nothing -> error $ "Couldn't find function at addr: " <> show (fn ^. #address)
    Just fn' -> return fn'

toBlazeFunction :: G.Function -> IO Function
toBlazeFunction gfunc = runGhidra $ do
  let fn = gfunc ^. #handle
  name <- G.getName fn
  isVariadic <- G.hasVarArgs fn
  paramInfos <- fmap convertParam <$> G.getParams fn
  -- Don't really know how to tell if individual params are varargs
  -- so for now, if isVariadic is true, we use FuncVarArgInfo
  let params = bool FuncParamInfo FuncVarArgInfo isVariadic <$> paramInfos
  return
    Func.Function
      { symbol = Nothing
      , name = name
      , address = getFuncAddr gfunc
      , params = params
      }

getFunction_ :: GhidraState -> Address -> IO (Maybe J.Function)
getFunction_ gs addr = runGhidra $ do
  jaddr <- State.mkAddress gs addr
  G.fromAddr gs jaddr

-- | Gets a Function based on address. Assumes function is in "ram"
-- Ghidra might not agree with your base addr, depending on if your
-- binary is PIEd. See internal issue #5
getFunction :: GhidraState -> Address -> IO (Maybe Function)
getFunction gs addr = getFunction_ gs addr >>= \case
  Nothing -> return Nothing
  Just jfunc -> Just <$> (G.mkFunction jfunc >>= toBlazeFunction)
  
getFunctions :: GhidraState -> IO [Function]
getFunctions gs = runGhidra $ G.getFunctions' opts gs
  >>= traverse G.mkFunction
  >>= traverse toBlazeFunction
  where
    -- Are these sensible options for blaze?
    opts = G.GetFunctionsOptions
      { G.includeExternalFuncs = False
      , G.includeLocalFuncs = True
      , G.excludeDefaultFuncs = False
      , G.excludeThunks = False
      , G.resolveThunks = False
      }

getCallSites :: GhidraState -> Function -> IO [CallSite]
getCallSites gs fn = runGhidra $ do
  startAddr <- State.mkAddress gs $ fn ^. #address
  G.fromAddr gs startAddr >>= \case
    Nothing -> error "Could not find callee function"
    Just gfunc -> GRef.getFunctionRefs gs gfunc >>= traverse f
  where
    f :: GRef.FuncRef -> IO CallSite
    f x = do
      caller <- toBlazeFunction $ x ^. #caller
      let addr = convertAddress $ x ^. #callerAddr
          dest = CG.DestFunc fn
      return $ CallSite { CG.caller = caller
                        , CG.address = addr
                        , CG.dest = dest
                        }
