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

{#enum BNMediumLevelILOperation {} deriving (Eq, Ord, Read, Show)#}

{#enum BNTypeClass {} deriving (Eq, Ord, Read, Show)#}

{#enum BNVariableSourceType {} deriving (Eq, Ord, Read, Show)#}

{#enum BNBranchType {} deriving (Eq, Ord, Read, Show)#}

{#enum BNEndianness {} deriving (Eq, Ord, Read, Show)#}
