{-# LANGUAGE TemplateHaskell #-}

module Binja.Types.Analysis where

import Binja.Prelude

import Binja.C.Enums (BNAnalysisMode)


data BNAnalysisParameters = 
  BNAnalysisParameters
  { _maxAnalysisTime :: Word64
  , _maxFunctionSize :: Word64
  , _maxFunctionAnalysisTime :: Word64
  , _maxFunctionUpdateCount :: Word64
  , _maxFunctionSubmitCount :: Word64
  , _suppressNewAutoFunctionAnalysis :: Bool 
  , _mode :: BNAnalysisMode
  , _alwaysAnalyzeIndirectBranches :: Bool 
  , _advancedAnalysisCacheSize :: Word64
  } 
  deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''BNAnalysisParameters)