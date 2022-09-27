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
import Control.Monad.Extra (mapMaybeM)
import Data.BinaryAnalysis (Symbol (Symbol, _symbolName, _symbolRawName))
import qualified Data.Set as Set
import qualified Data.Text as Text

import Ghidra.Core (runGhidra)
import Ghidra.State (GhidraState)
import qualified Ghidra.State as State
import qualified Ghidra.Function as G
import qualified Ghidra.Types.Function as G
import qualified Ghidra.Reference as GRef


getFuncAddr :: G.Function -> Address
getFuncAddr = convertAddress . view #startAddress

convertParam :: G.Parameter -> ParamInfo
convertParam p = ParamInfo (p ^. #name) BFunc.Unknown

convertFunction :: G.Function -> IO Function
convertFunction gfunc = runGhidra $ do
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

-- | Gets a Function based on address. Assumes function is in "ram"
-- Ghidra might not agree with your base addr, depending on if your
-- binary is PIEd. See internal issue #5
getFunction :: GhidraState -> Address -> IO (Maybe Function)
getFunction gs addr = runGhidra $ do
  jaddr <- State.mkAddress gs addr
  G.fromAddr gs jaddr >>= \case
    Nothing -> return Nothing
    Just jfunc -> Just <$> (G.mkFunction jfunc >>= convertFunction)
  
getFunctions :: GhidraState -> IO [Function]
getFunctions gs = runGhidra $ G.getFunctions' opts gs
  >>= traverse G.mkFunction
  >>= traverse convertFunction
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
      caller <- convertFunction $ x ^. #caller
      let addr = convertAddress $ x ^. #callerAddr
          dest = CG.DestFunc fn
      return $ CallSite { CG.caller = caller
                        , CG.address = addr
                        , CG.dest = dest
                        }
