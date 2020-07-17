{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.IndirectCallResolution where
    
import Blaze.Prelude
import qualified Blaze.Types.CallGraph as Cg
import Blaze.Types.Pil (Stmt)
import Blaze.Types.VTable (VTable)
import Blaze.Types.Function (CallInstruction)

data ClassConstructor = ClassConstructor
    { _cFunction :: Cg.Function
    , _instruction :: Stmt
    , _vtable :: VTable
    } deriving (Eq, Show)

data IndirectCall = IndirectCall
    { _iFunction :: Cg.Function
    , _callInstr :: CallInstruction
    } deriving (Eq, Show)

$(makeFieldsNoPrefix ''ClassConstructor)
$(makeFieldsNoPrefix ''IndirectCall)
