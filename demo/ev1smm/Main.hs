module Main (main) where

import Flint.Prelude

import Flint.Types.Analysis
import Flint.Analysis
import Flint.Analysis.Path.Matcher
import Flint.Types.Query

import qualified Binja.Core as BN
import Blaze.Import.Source.BinaryNinja (BNImporter)
import qualified Blaze.Types.Pil as Pil

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

-- This example shows a failed check where they check the commbuffer size (arg4)
-- but later access outside its range. Pseudo-pattern:
--
-- bind 'y' = [arg4]
-- if (bind 'y' < bind 'length')
-- call SmmIsBufferOutsideSmmValid(arg3, bind 'y')
-- [arg3 + (bind 'n')] = (bind "store_val") _ where (bind 'n' > bind 'length')


commBufferOobWriteBug :: BugMatch
commBufferOobWriteBug = BugMatch
  { pathPattern =
      [ Stmt $ Def (Bind "y" Wild) (Expr . Pil.LOAD . Pil.LoadOp $ Var "arg4")
      , Unordered
        [ AnyOne
          [ Stmt $ BranchCond . Expr . Pil.CMP_ULT $ Pil.CmpUltOp (Bind "y" Wild) (Bind "max_length" Wild)
          , Stmt $ BranchCond . Expr . Pil.CMP_UGE $ Pil.CmpUgeOp (Bind "max_length" Wild) (Bind "y" Wild)
          ]
        , Stmt $ Call (Just Wild) (CallFunc (FuncName "SmmIsBufferOutsideSmmValid"))
          [ Var "arg3"
          , Bind "y" Wild
          ]
        ]
      , Stmt $ Store (Expr . Pil.ADD $ Pil.AddOp (Var "arg3") (Bind "n" Wild)) (Bind "stored_val" Wild)
      , Assert . BoundExpr (ConstSize 8) . Pil.CMP_UGT $ Pil.CmpUgtOp (Bound "n") (Bound "max_length")
      ]
  , bugName = "CommBuffer Out of Bounds Write"
  , bugDescription =
    "In this path, the CommBuffer size (arg4) is properly copied to a local variable, then checked to be less than `" <> TextExpr "max_length" <> "` and checked to reside outside SMRAM by calling `SmmIsBufferOutsideSmmValid`, but the CommBuffer (arg3) is later written to at offset `" <> TextExpr "n" <> "`, which could be greater than `" <> TextExpr "max_length" <> "`. This allows an attacker to possibly overwrite the beginning of SMRAM."
  , mitigationAdvice = "Ensure that the value of `arg4` is checked to be smaller than the maximum size of the `CommBuffer`."
  }

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
