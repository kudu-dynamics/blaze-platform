module Blaze.Types.IndirectCallResolution where

import Blaze.Prelude
import Blaze.Types.Function (Function)
import Blaze.Types.Pil (Stmt, CallStmt)
import Blaze.Types.VTable (VTable)

data ClassConstructor = ClassConstructor
    { function :: Function
    , vptrStore :: Stmt
    , vtable :: VTable
    } deriving (Eq, Show, Generic)

data IndirectCall = IndirectCall
    { function :: Function
    , callInstr :: CallStmt
    } deriving (Eq, Show, Generic)
