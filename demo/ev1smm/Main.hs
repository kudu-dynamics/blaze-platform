module Main (main) where

import Flint.Prelude

import Flint.Types.Analysis
import Flint.Analysis
import Flint.Analysis.Path.Matcher
import Flint.Types.Query

import qualified Binja.Core as BN
import Blaze.Import.Source.BinaryNinja (BNImporter)

import System.Directory (listDirectory)
import qualified Data.HashSet as HashSet


-- variableSmmRuntimeDxeConfig :: BinarySearchConfig BNImporter FuncConfig
-- variableSmmRuntimeDxeConfig = BinarySearchConfig
--   { binaryPath = "/tmp/e1smm/VariableSmmRuntimeDxe.bndb"
--   , excludeFuncsFromStore = []
--   , queries =
--     [
--       QueryAllPaths $ QueryAllPathsOpts
--       { start = FuncSym "RuntimeServiceQueryVariableInfo"
--       }
--     ,
--       QueryAllPaths $ QueryAllPathsOpts
--       { start = FuncSym "RuntimeServiceGetVariable"
--       }
--     ]    
--   }

-- blSupportSmmConfig :: BinarySearchConfig BNImporter FuncConfig
-- blSupportSmmConfig = BinarySearchConfig
--   { binaryPath = "/tmp/e1smm/BlSupportSmm.bndb"
--   , excludeFuncsFromStore = []
--   , queries =
--     [
--       QueryAllPaths $ QueryAllPathsOpts
--       { start = FuncSym "BlSwSmiHandler"
--       }    
--     ]    
--   }


-- variableSmmConfig :: BinarySearchConfig BNImporter FuncConfig
-- variableSmmConfig = BinarySearchConfig
--   { binaryPath = "/tmp/e1smm/VariableSmm.bndb"
--   , excludeFuncsFromStore = []
--   , queries =
--     [
--       -- QueryExploreDeep $ QueryExploreDeepOpts
--       -- { start = FuncSym "VariableServiceQueryVariableInfoInternal"
--       -- , callExpandDepthLimit = 10
--       -- , numSamples = 500
--       -- },
--       -- QueryExploreDeep $ QueryExploreDeepOpts
--       -- { start = FuncSym "VariableServiceQueryVariableInfo"
--       -- , callExpandDepthLimit = 1
--       -- , numSamples = 50
--       -- },

--       -- QueryAllPaths $ QueryAllPathsOpts
--       -- { start = FuncSym "VariableServiceQueryVariableInfoInternal"
--       -- },

--       QueryAllPaths $ QueryAllPathsOpts
--       { start = FuncSym "VariableServiceSetVariable"
--       }

    
--     ]    
--   }

-- lockboxSmmConfig :: BinarySearchConfig BNImporter FuncConfig
-- lockboxSmmConfig = BinarySearchConfig
--   { binaryPath = "/tmp/e1smm/SmmLockBox.bndb"
--   , excludeFuncsFromStore = []
--   , queries =
--     [
--       QueryTarget $ QueryTargetOpts
--       { start = FuncSym "SmmLockBoxHandler"
--       , mustReachSome = (FuncSym "RestoreLockBox", 0x175c) :| []
--       , callExpandDepthLimit = 20
--       , numSamples = 200
--       }
--     -- ,
--       -- QueryTarget $ QueryTargetOpts
--       -- { start = FuncSym "SmmLockBoxHandler"
--       -- , mustReachSome = (FuncSym "CopyMem", 0x166b) :| []
--       -- , callExpandDepthLimit = 20
--       -- , numSamples = 300
--       -- }
--     ]    
--   }

-- rwVariableConfig :: BinarySearchConfig BNImporter FuncConfig
-- rwVariableConfig = BinarySearchConfig
--   { binaryPath = "/tmp/e1smm/RWVariable.debug.bndb"
--   , excludeFuncsFromStore = []
--   , queries =
--     [
--       QueryAllPaths $ QueryAllPathsOpts
--       { start = FuncSym "RWVariableHandler"
--       }
--     -- ,
--       -- QueryTarget $ QueryTargetOpts
--       -- { start = FuncSym "SmmLockBoxHandler"
--       -- , mustReachSome = (FuncSym "CopyMem", 0x166b) :| []
--       -- , callExpandDepthLimit = 20
--       -- , numSamples = 300
--       -- }
--     ]    
--   }

failedToCheckCommBufferIsOutsideSmramPattern :: [StmtPattern]
failedToCheckCommBufferIsOutsideSmramPattern =
  [ AvoidUntil $ AvoidSpec
    (Stmt $ Call Nothing (CallFunc (FuncName "SmmIsBufferOutsideSmmValid")) [Var "arg3", Wild])
    Nothing
  , AnyOne [ Stmt $ Store (Contains (Var "arg3")) Wild
           , Stmt $ Call Nothing (CallFunc (FuncName "CopyMem")) [Contains (Var "arg3"), Wild, Wild]
           ]
  ]

failedToCheckCommBufferIsOutsideSmramBug :: BugMatch
failedToCheckCommBufferIsOutsideSmramBug = BugMatch
  { pathPattern = failedToCheckCommBufferIsOutsideSmramPattern
  , bugName = "Arbitrary Write to SMRAM"
  , bugDescription = "This path writes a value to an address in the user-controlled CommBuffer (arg3) without first checking to make sure arg3 is pointing outside of SMRAM, allowing a user to overwrite SMRAM."
  , mitigationAdvice = "Before writing to the CommBuffer in any way, SmmIsBufferOutsideSmmValid should be called on the user-supplied pointer to the CommBuffer to ensure it is not pointing inside SMRAM."
  }


smmCalloutPattern :: [StmtPattern]
smmCalloutPattern =
  [ AnyOne
    [ Stmt $ Call Nothing (CallIndirect . Contains $ Var "gRT") []
    , Stmt $ Call Nothing (CallIndirect . Contains $ Var "gBS") []
    ]
  ]

smmCalloutBug :: BugMatch
smmCalloutBug = BugMatch
  { pathPattern =
    [ AnyOne
      [ nonSmmTableCall "gRT"
      , nonSmmTableCall "gBS"
      ]
    ]
  , bugName = "SMM Callout"
  , bugDescription =
    "This path makes an indirect call to `" <> TextExpr "fullAddr" <> "` from within an SMI handler. Because `" <> TextExpr "globalTable" <> "` is stored in OS memory, ring 0 is able to modify pointers in the function tables to gain arbitrary code execution."
  , mitigationAdvice = "Instead of calling `" <> TextExpr "globalTable" <> "`, use EFI_SYSTEM_TABLE *gSmst from SmmServicesTableLib (for traditional MM modules) or EFI_MM_SYSTEM_TABLE *gMmst from MmServicesTableLib (for standalone MM modules)."
  }
  where
    nonSmmTableCall tableName =
      Stmt $ Call Nothing
      (CallIndirect . Bind "fullAddr" . Contains . Bind "globalTable" $ Var tableName)
      []

queryVariableInfoConfig :: BinarySearchConfig BNImporter FuncConfig
queryVariableInfoConfig = BinarySearchConfig
  { binaryPath = "/tmp/e1smm/QueryVariableInfo.debug.bndb"
  , excludeFuncsFromStore = []
  , queries =
    [
      ( QueryAllPaths $ QueryAllPathsOpts
        { start = FuncSym "QueryVariableInfoHandler"
        }
      , [ smmCalloutBug
        ]
      )
    ]    
  }

rwVariableConfig :: BinarySearchConfig BNImporter FuncConfig
rwVariableConfig = BinarySearchConfig
  { binaryPath = "/tmp/e1smm/RWVariable.debug.bndb"
  , excludeFuncsFromStore = []
  , queries =
    [
      ( QueryAllPaths $ QueryAllPathsOpts
        { start = FuncSym "RWVariableHandler"
        }
      , [ smmCalloutBug
        , failedToCheckCommBufferIsOutsideSmramBug
        ]
      )
    ]    
  }

main :: IO ()
main = do
  putText "starting"
  summariesOfInterest queryVariableInfoConfig
  -- summariesOfInterest rwVariableConfig
  putText "finished"
