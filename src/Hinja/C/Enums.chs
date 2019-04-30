{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Hinja.C.Enums where

import Hinja.Prelude

import qualified Prelude as P
import Prelude (error)

#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#include "/tmp/beauty/binaryninjacore.h"

{#context lib="binaryninjacore" #}

{#enum BNMediumLevelILOperation {} deriving (Eq, Ord, Read, Show)#}

{#enum BNTypeClass {} deriving (Eq, Ord, Read, Show)#}

{#enum BNVariableSourceType {} deriving (Eq, Ord, Read, Show)#}
