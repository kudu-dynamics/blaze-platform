module Main (main) where

import Flint.Prelude

import Flint.Types.Analysis
import Flint.Analysis
import Flint.Analysis.Path.Matcher
import Flint.Types.Query
import qualified Flint.Analysis.Uefi as Uefi

import qualified Binja.Core as BN
import Blaze.Import.Source.BinaryNinja (BNImporter)
import qualified Blaze.Types.Pil as Pil

import System.Directory (listDirectory)
import qualified Data.HashSet as HashSet



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
  { pathPattern =
    [ AvoidUntil $ AvoidSpec
      (Stmt $ Call Nothing (CallFunc (FuncName "SmmIsBufferOutsideSmmValid")) [Var "arg3", Wild])
      Nothing
    , AnyOne [ Stmt $ Store (Bind "fullAddr" (Contains (Var "arg3"))) (Bind "value" Wild)
             , Stmt $ Call Nothing (CallFunc (FuncName "CopyMem")) [Bind "fullAddr" (Contains (Var "arg3")), (Bind "value" Wild), Wild]
             ]
    ]
  , bugName = "Arbitrary Write to SMRAM"
  , bugDescription = "This path writes a value `" <> TextExpr "value" <> "' to the address `" <> TextExpr "fullAddr" <> "`, which contains the user-controlled CommBuffer (arg3), without first checking to make sure arg3 is pointing outside of SMRAM, allowing a user to overwrite SMRAM."
  , mitigationAdvice = "Before writing to the CommBuffer in any way, SmmIsBufferOutsideSmmValid should be called on the user-supplied pointer to the CommBuffer to ensure it is not pointing inside SMRAM."
  }

failedToCheckCommBufferIsOutsideSmramArbitraryReadBug :: BugMatch
failedToCheckCommBufferIsOutsideSmramArbitraryReadBug =
  let getVariable = Expr . Pil.LOAD . Pil.LoadOp
                    . Expr . Pil.LOAD . Pil.LoadOp $ Var "mSmmVariable"
      setVariable = Expr . Pil.LOAD . Pil.LoadOp
                    . Expr . Pil.ADD $ Pil.AddOp
                             (Expr . Pil.LOAD . Pil.LoadOp $ Var "mSmmVariable")
                             (Expr . Pil.CONST . Pil.ConstOp $ 0x10)
  in
    BugMatch
    { pathPattern =
      [ AvoidUntil $ AvoidSpec
        (Stmt $ Call Nothing (CallFunc (FuncName "SmmIsBufferOutsideSmmValid")) [Var "arg3", Wild])
        Nothing
      , Stmt $ Call Nothing (CallIndirect setVariable) [Wild, Wild, Wild, Wild, Bind "srcBuffer" $ Contains (Var "arg3")]
      ]
    , bugName = "Arbitrary Read from SMRAM through NV Variable"
    , bugDescription = "In this path, the call to `setVariable` fills the contents of a NV variable with the contents of a buffer at location `" <> TextExpr "srcBuffer" <> "', which is at least partially controlled by the user-supplied CommBuffer (arg3), possibly allowing a user to read memory from SMRAM."
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

smmLightConfig :: BinarySearchConfig BNImporter FuncConfig
smmLightConfig = BinarySearchConfig
  { binaryPath = "/tmp/e1smm/HwSmmLight.debug.bndb"
  , excludeFuncsFromStore = []
  , queries =
    [
      ( QueryAllPaths $ QueryAllPathsOpts
        { start = FuncSym "UnregisterCBFunctionLight"
        }
      , [ smmCalloutBug
        , failedToCheckCommBufferIsOutsideSmramArbitraryReadBug
        , failedToCheckCommBufferIsOutsideSmramBug
        , commBufferOobWriteBug
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
        , failedToCheckCommBufferIsOutsideSmramArbitraryReadBug
        ]
      )
    ]
  }

main :: IO ()
main = do
  putText "starting"
  -- summariesOfInterest queryVariableInfoConfig
  -- summariesOfInterest rwVariableConfig
  summariesOfInterest Uefi.taintPropagators smmLightConfig
  putText "finished"
