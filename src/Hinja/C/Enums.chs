{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Hinja.C.Enums where

import Hinja.Prelude hiding (from, to)

import Prelude (error)

#include <binaryninjacore.h>

{#context lib="binaryninjacore" #}

{#enum BNMediumLevelILOperation {} deriving (Eq, Ord, Read, Show)#}

{#enum BNTypeClass {} deriving (Eq, Ord, Read, Show)#}

{#enum BNVariableSourceType {} deriving (Eq, Ord, Read, Show)#}

{#enum BNBranchType {} deriving (Eq, Ord, Read, Show)#}
