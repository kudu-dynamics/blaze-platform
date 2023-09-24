module Main (main) where

import Flint.Prelude

import Flint.Types.Analysis
import Flint.Analysis
import Flint.Types.Query

import qualified Binja.Core as BN
import Blaze.Import.Source.BinaryNinja (BNImporter)

import System.Directory (listDirectory)
import qualified Data.HashSet as HashSet


variableSmmRuntimeDxeConfig :: BinarySearchConfig BNImporter FuncConfig
variableSmmRuntimeDxeConfig = BinarySearchConfig
  { binaryPath = "/tmp/e1smm/VariableSmmRuntimeDxe.bndb"
  , excludeFuncsFromStore = []
  , queries =
    [
      QueryAllPaths $ QueryAllPathsOpts
      { start = FuncSym "RuntimeServiceQueryVariableInfo"
      }
    ,
      QueryAllPaths $ QueryAllPathsOpts
      { start = FuncSym "RuntimeServiceGetVariable"
      }
    ]    
  }

blSupportSmmConfig :: BinarySearchConfig BNImporter FuncConfig
blSupportSmmConfig = BinarySearchConfig
  { binaryPath = "/tmp/e1smm/BlSupportSmm.bndb"
  , excludeFuncsFromStore = []
  , queries =
    [
      QueryAllPaths $ QueryAllPathsOpts
      { start = FuncSym "BlSwSmiHandler"
      }    
    ]    
  }


variableSmmConfig :: BinarySearchConfig BNImporter FuncConfig
variableSmmConfig = BinarySearchConfig
  { binaryPath = "/tmp/e1smm/VariableSmm.bndb"
  , excludeFuncsFromStore = []
  , queries =
    [
      -- QueryExploreDeep $ QueryExploreDeepOpts
      -- { start = FuncSym "VariableServiceQueryVariableInfoInternal"
      -- , callExpandDepthLimit = 10
      -- , numSamples = 500
      -- },
      -- QueryExploreDeep $ QueryExploreDeepOpts
      -- { start = FuncSym "VariableServiceQueryVariableInfo"
      -- , callExpandDepthLimit = 1
      -- , numSamples = 50
      -- },

      -- QueryAllPaths $ QueryAllPathsOpts
      -- { start = FuncSym "VariableServiceQueryVariableInfoInternal"
      -- },

      QueryAllPaths $ QueryAllPathsOpts
      { start = FuncSym "VariableServiceSetVariable"
      }

    
    ]    
  }

lockboxSmmConfig :: BinarySearchConfig BNImporter FuncConfig
lockboxSmmConfig = BinarySearchConfig
  { binaryPath = "/tmp/e1smm/SmmLockBox.bndb"
  , excludeFuncsFromStore = []
  , queries =
    [
      QueryTarget $ QueryTargetOpts
      { start = FuncSym "SmmLockBoxHandler"
      , mustReachSome = (FuncSym "RestoreLockBox", 0x175c) :| []
      , callExpandDepthLimit = 20
      , numSamples = 200
      }
    -- ,
      -- QueryTarget $ QueryTargetOpts
      -- { start = FuncSym "SmmLockBoxHandler"
      -- , mustReachSome = (FuncSym "CopyMem", 0x166b) :| []
      -- , callExpandDepthLimit = 20
      -- , numSamples = 300
      -- }
    ]    
  }

rwVariableConfig :: BinarySearchConfig BNImporter FuncConfig
rwVariableConfig = BinarySearchConfig
  { binaryPath = "/tmp/e1smm/RWVariable.debug.bndb"
  , excludeFuncsFromStore = []
  , queries =
    [
      QueryAllPaths $ QueryAllPathsOpts
      { start = FuncSym "RWVariableHandler"
      }
    -- ,
      -- QueryTarget $ QueryTargetOpts
      -- { start = FuncSym "SmmLockBoxHandler"
      -- , mustReachSome = (FuncSym "CopyMem", 0x166b) :| []
      -- , callExpandDepthLimit = 20
      -- , numSamples = 300
      -- }
    ]    
  }

main :: IO ()
main = do
  putText "starting"
  -- summariesOfInterest variableSmmConfig
  -- summariesOfInterest lockboxSmmConfig
  -- summariesOfInterest blSupportSmmConfig
  -- summariesOfInterest variableSmmRuntimeDxeConfig
  summariesOfInterest rwVariableConfig
  putText "finished"
