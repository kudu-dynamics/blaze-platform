{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Binja.C.Enums where

import Binja.Prelude hiding (from, to)

import Prelude (error)

#include <binaryninjacore.h>

{#context lib="binaryninjacore" #}

{#enum BNAnalysisMode {} deriving (Eq, Ord, Read, Show)#}

{#enum BNAnalysisSkipReason {} deriving (Eq, Ord, Read, Show)#}

{#enum BNBranchType {} deriving (Eq, Ord, Read, Show)#}

{#enum BNEndianness {} deriving (Eq, Ord, Read, Show)#}

{#enum BNFunctionAnalysisSkipOverride {} deriving (Eq, Ord, Read, Show)#}

{#enum BNMediumLevelILOperation {} deriving (Eq, Ord, Read, Show)#}

{#enum BNStringType {} deriving (Eq, Ord, Read, Show)#}

{#enum BNTypeClass {} deriving (Eq, Ord, Read, Show)#}

{#enum BNVariableSourceType {} deriving (Eq, Ord, Read, Show)#}