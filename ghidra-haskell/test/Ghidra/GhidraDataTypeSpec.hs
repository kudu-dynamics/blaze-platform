module Ghidra.GhidraDataTypeSpec where

import Test.Hspec
import Ghidra.Prelude
import qualified Ghidra.State as State
import Ghidra.State (GhidraState)
import Ghidra.Core
import qualified Ghidra.Types as J
import Ghidra.Function (getHighFunction, getFunctions', GetFunctionsOptions(..))
import qualified Data.HashMap.Strict as HM
import Ghidra.GhidraDataTypes


annotatedTestBin :: FilePath
annotatedTestBin = "../blaze/res/test_bins/annotated_binary/annotated_binary.gzf"

recursiveTestBin :: FilePath
recursiveTestBin = "../blaze/res/test_bins/recursive_struct/recursive_struct.gzf"

getFuncOpts :: GetFunctionsOptions
getFuncOpts = GetFunctionsOptions
  { includeExternalFuncs = False
  , includeLocalFuncs = True
  , excludeDefaultFuncs = False
  , excludeThunks = True
  , resolveThunks = False
  }

data TestCtx = TestCtx 
  { annotatedGhidraState :: State.GhidraState
  , annotatedFuncs :: [J.Function]
  , recursiveGhidraState :: State.GhidraState
  , recursiveFuncs :: [J.Function]
  } deriving (Generic)

getGhidraState :: FilePath -> Ghidra GhidraState
getGhidraState fp = State.openDatabase_ fp >>! State.analyze

getTestCtx :: IO TestCtx
getTestCtx = do
  annotatedGhidraState <- runGhidraOrError $ getGhidraState annotatedTestBin
  let annotatedProg = annotatedGhidraState ^. #program
  annotatedFuncs <- runGhidraOrError $ getFunctions' getFuncOpts annotatedProg

  recursiveGhidraState <- runGhidraOrError $ getGhidraState recursiveTestBin
  let recursiveProg = recursiveGhidraState ^. #program
  recursiveFuncs <- runGhidraOrError $ getFunctions' getFuncOpts recursiveProg

  return $ TestCtx
      { annotatedGhidraState = annotatedGhidraState
      , annotatedFuncs = annotatedFuncs
      , recursiveGhidraState = recursiveGhidraState
      , recursiveFuncs = recursiveFuncs
      }


spec :: Spec
spec = beforeAll getTestCtx . describe "Ghidra.GhidraDataType" $ do
    context "testing the ability of getting type information from Ghidra" $ do
        it "correctly converts types" $ \tctx -> do
          let action = do
                let prog = tctx ^. (#annotatedGhidraState . #program)
                let calculateSecreteAttributeFunc = (tctx ^. #annotatedFuncs)!!calculateSecreteAttributeIndex
                calculateSecreteAttributeHFunc <- runGhidraOrError $ getHighFunction (tctx ^. #annotatedGhidraState) calculateSecreteAttributeFunc
                lst1 <- runGhidraOrError $ getSymAndTypeList prog calculateSecreteAttributeHFunc calculateSecreteAttributeFuncNameList
                let map1 = (prune . HM.fromList) lst1
                print map1
                print calculateSecreteAttributeMap

                let entryFunc = (tctx ^. #annotatedFuncs)!!entryIndex
                entryHFunc <- runGhidraOrError $ getHighFunction (tctx ^. #annotatedGhidraState) entryFunc
                lst2 <- runGhidraOrError $ getSymAndTypeList prog entryHFunc entryFuncNameList
                let map2 = (prune . HM.fromList) lst2
                print map2
                print entryFuncMap
                
{-
                let entryFuncRecursive = (tctx ^. #recursiveFuncs)!!entryIndexRecursive
                entryHFuncRecursive <- runGhidraOrError $ getHighFunction (tctx ^. #recursiveGhidraState) entryFuncRecursive
                lst3 <- runGhidraOrError $ getSymAndTypeList entryHFuncRecursive entryFuncNameListRecursive
                let map3 = (prune . HM.fromList) lst3
-}

                return $ (calculateSecreteAttributeMap == map1) && (entryFuncMap == map2)
          action `shouldReturn` True


calculateSecreteAttributeMap :: HashMap Text GhidraDataType
calculateSecreteAttributeMap =
  HM.fromList
    [ ("temp"
    , ArrayType (ArrayTypeOpts 
        { elementType = FloatType (FloatTypeOpts {width = Bytes 8})
        , elementWidth = Bytes 8, len = 72
        })
      )
    , ("param_1"
      , PointerType (PointerTypeOpts 
          { width = Bytes 8, 
            pointeeType = StructType (StructTypeOpts 
                            { width = Bytes 62
                            , fields = 
                                [ (0,IntType (IntTypeOpts { width = Bytes 4, isSigned = True }))
                                , (4,FloatType (FloatTypeOpts { width = Bytes 4 }))
                                , (8,FloatType (FloatTypeOpts { width = Bytes 8 }))
                                , (16,BoolType (BoolTypeOpts { width = Bytes 1 }))
                                , (17,CharType (CharTypeOpts { width = Bytes 1 }))
                                , (18,ArrayType (ArrayTypeOpts { elementType = IntType (IntTypeOpts {width = Bytes 4, isSigned = True })
                                                               , elementWidth = Bytes 4, len = 36 }))
                                , (54,PointerType (PointerTypeOpts {width = Bytes 8, pointeeType = CharType (CharTypeOpts {width = Bytes 1}) }))
                                ]
                            })
          })
      )
    ]

entryFuncMap :: HashMap Text GhidraDataType
entryFuncMap = 
  HM.fromList
    [ ("person"
    ,  StructType (StructTypeOpts 
        { width = Bytes 62
        , fields =
            [ (0,IntType (IntTypeOpts {width = Bytes 4, isSigned = True}))
            , (4,FloatType (FloatTypeOpts {width = Bytes 4}))
            , (8,FloatType (FloatTypeOpts {width = Bytes 8}))
            , (16,BoolType (BoolTypeOpts {width = Bytes 1}))
            , (17,CharType (CharTypeOpts {width = Bytes 1}))
            , (18,ArrayType (ArrayTypeOpts {elementType = IntType (IntTypeOpts {width = Bytes 4, isSigned = True}), elementWidth = Bytes 4, len = 36}))
            , (54,PointerType (PointerTypeOpts {width = Bytes 8, pointeeType = CharType (CharTypeOpts {width = Bytes 1})}))
            ]
        })
      )
    , ("string"
      , ArrayType (ArrayTypeOpts 
          { elementType = CharType (CharTypeOpts {width = Bytes 1})
          , elementWidth = Bytes 1, len = 50
          })
      )
    ]

calculateSecreteAttributeFuncNameList :: [Text]
calculateSecreteAttributeFuncNameList = ["temp", "param_1"]

entryFuncNameList :: [Text]
entryFuncNameList = ["person", "string"]

entryFuncNameListRecursive :: [Text]
entryFuncNameListRecursive = ["node"]

calculateSecreteAttributeIndex :: Int
calculateSecreteAttributeIndex = 0

entryIndex :: Int
entryIndex = 2

entryIndexRecursive :: Int
entryIndexRecursive = 2


getSymAndTypeList :: J.ProgramDB -> J.HighFunction -> [Text] -> Ghidra [(Text, Maybe GhidraDataType)]
getSymAndTypeList prog hFunc = mapM (\symbol -> do
                              dt <- getDataTypeFromSymAndHighFunc prog symbol hFunc
                              return (symbol, dt))

prune :: HM.HashMap k (Maybe v) -> HM.HashMap k v 
prune = HM.mapMaybe id
  where
    id :: a -> a
    id x = x




