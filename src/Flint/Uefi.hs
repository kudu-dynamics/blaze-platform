module Flint.Uefi
  ( module Flint.Uefi
  ) where

import Blaze.Prelude hiding (getConst, Symbol, sym)

import Blaze.Pil (
    AddOp (AddOp),
    CallDest (CallExpr),
    DefOp (DefOp),
    ExprOp (ADD, CALL, LOAD),
    Expression (Expression),
    ConstOp,
    LoadOp (LoadOp),
    Statement (Def),
    Stmt,
    Symbol,
 )
import Blaze.Pil qualified as Pil

import Control.Lens (findOf)
import qualified Data.HashMap.Strict as HMap

{- | Lookup table for boot services functions.
Maps offsets to names.
-}
bsNames :: HashMap Int Symbol
bsNames =
    HMap.fromList
        [ (0x18, "RaiseTPL")
        , (0x20, "RestoreTPL")
        , (0x28, "AllocatePages")
        , (0x30, "FreePages")
        , (0x38, "GetMemoryMap")
        , (0x40, "AllocatePool")
        , (0x48, "FreePool")
        , (0x50, "CreateEVent")
        , (0x58, "SetTimer")
        , (0x60, "WaitForEvent")
        , (0x68, "SignalEvent")
        , (0x70, "CloseEvent")
        , (0x78, "CheckEvent")
        , (0x80, "InstallProtocolInterface")
        , (0x88, "ReinstallProtocolInterface")
        , (0x90, "UninstallProtocolInterface")
        , (0x98, "HandleProtocol")
        , (0xa0, "Reserved")
        , (0xa8, "RegisterProtocolNotify")
        , (0xb0, "LocateHandle")
        , (0xb8, "LocateDevicePath")
        , (0xc0, "InstallConfigurationTable")
        , (0xc8, "LoadImage")
        , (0xd0, "StartImage")
        , (0xd8, "Exit")
        , (0xe0, "UnloadImage")
        , (0xe8, "ExitBootServices")
        , (0xf0, "GetNextMonotonicCount")
        , (0xf8, "Stall")
        , (0x100, "SetWatchdogTimer")
        , (0x108, "ConnectController")
        , (0x110, "DisconnectController")
        , (0x118, "OpenProtocol")
        , (0x120, "CloseProtocol")
        , (0x128, "OpenProtocolInformation")
        , (0x130, "ProtocolsPerHandle")
        , (0x138, "LocateHandleBuffer")
        , (0x140, "LocateProtocol")
        , (0x148, "InstallMultipleProtocolInterfaces")
        , (0x150, "UninstallMultipleProtocolInterfaces")
        , (0x158, "CalculateCrc32")
        , (0x160, "CopyMem")
        , (0x168, "SetMem")
        , (0x170, "CreateEventEx")
        ]

{- | Lookup table for runtime services functions.
Maps offsets to names.
-}
rtNames :: HashMap Int Symbol
rtNames =
    HMap.fromList
        [ (0x18, "GetTime")
        , (0x20, "SetTime")
        , (0x28, "GetWakeupTime")
        , (0x30, "SetWakeupTime")
        , (0x38, "SetVirtualAddressMap")
        , (0x40, "ConvertPointer")
        , (0x48, "GetVariable")
        , (0x50, "GetNextVariableName")
        , (0x58, "SetVariable")
        , (0x60, "GetAccessVariable")
        , (0x68, "SetAccessVariable")
        , (0x70, "GetNextHighMonotonicCount")
        , (0x78, "ResetSystem")
        , (0x80, "UpdateCapsule")
        , (0x88, "QueryCapsuleCapabilities")
        , (0x90, "QueryVariableInfo")
        ]

-- | Lookup table for system table.
stNames :: HashMap Int Symbol
stNames =
    HMap.fromList
        [ (0x58, "RuntimeServices")
        ]

isCall :: Stmt -> Bool
isCall (Def (DefOp _var (Expression _sz (CALL _callOp)))) = True
isCall _ = False

getCallExpr :: Stmt -> Maybe Expression
getCallExpr stmt = do
  callStmt <- Pil.mkCallStatement stmt
  case Pil.getCallDest callStmt of
    CallExpr expr -> Just expr
    _ -> Nothing

getBsOffset :: ExprOp Expression -> Maybe (ConstOp Expression)
getBsOffset (LOAD (LoadOp
                   (Expression _ (ADD (AddOp
                                       (Expression _ left)
                                       (Expression _ right))))))
  | Just _bs <- findOf (_Ctor @"LOAD" . #src . #op . _Ctor @"ConstFuncPtr")
                (\x -> x ^. #symbol == Just "gBS") left
  , Just offset <- right ^? _Ctor @"CONST"
  = Just offset
  | Just _bs <- findOf (_Ctor @"LOAD" . #src . #op . _Ctor @"ConstFuncPtr")
                (\x -> x ^. #symbol == Just "gBS") right
  , Just offset <- left ^? _Ctor @"CONST"
  = Just offset
getBsOffset _ = Nothing

updateBsName :: Expression -> Expression
updateBsName expr =
  case expr of
    (Expression _ (CALL callOp)) ->
      let name = do
            destExpr <- callOp ^? #dest . _Ctor @"CallExpr"
            offset <- getBsOffset $ destExpr ^. #op
            HMap.lookup (fromIntegral $ offset ^. #constant) bsNames
      in maybe expr (\x -> expr & #op . _Ctor @"CALL" . #name ?~ x) name
    _ -> expr

{- | Adds BS name to appropriate statements and returns the updated statement.
Otherwise returns the original statement.
-}
addBsName :: Stmt -> Stmt
addBsName stmt = updateBsName <$> stmt

resolveCalls :: [Stmt] -> [Stmt]
resolveCalls = fmap addBsName


-- {- | Find Def statement where the source expression is a load
-- from a known global.
-- -}
-- getRefVar :: Symbol -> Stmt -> Maybe PilVar
-- getRefVar sym stmt
--     (Def (DefOp
--            var
--            (Expression
--              _size
--              (LOAD (LoadOp addr))
--            )
--          )
--     ) =
--         if refAddr == addr
--             then Just var
--             else Nothing
-- getRefVar _ _ = Nothing

-- -- | Find all PIL vars that are defined as the result of a load
-- -- from a given static address.
-- findRefVars :: Address -> [Stmt] -> [PilVar]
-- findRefVars sym stmts = undefined
