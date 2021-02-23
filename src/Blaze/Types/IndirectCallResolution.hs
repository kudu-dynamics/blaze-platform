{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.IndirectCallResolution where
    
import Blaze.Prelude
import Blaze.Types.Function (Function)
import Blaze.Types.Pil (Stmt)
import Blaze.Types.VTable (VTable)
import Blaze.Import.Source.BinaryNinja.Types (CallInstruction)

data ClassConstructor = ClassConstructor
    { _cFunction :: Function
    , _instruction :: Stmt
    , _vtable :: VTable
    } deriving (Eq, Show)

data IndirectCall = IndirectCall
    { _iFunction :: Function
    , _callInstr :: CallInstruction
    } deriving (Eq, Show)

$(makeFieldsNoPrefix ''ClassConstructor)
$(makeFieldsNoPrefix ''IndirectCall)
