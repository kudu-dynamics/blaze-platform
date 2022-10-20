{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blaze.Prelude hiding ((<.>))

import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Printer
import qualified Data.Text.IO as TextIO
import System.FilePath ((</>), (<.>))


-- name -> [(field, type)]
ops :: HashMap Text [(Text, Text)]
ops = HashMap.fromList
  [ ("Adc", binOpWithCarry)
  , ("Add", binOp)
  , ("AddWillCarry", binOp)
  , ("AddWillOverflow", binOp)
  , ("And", binOp)
  , ("Asr", binOp)
  , ("BoolToInt", srcExpr)
  , ("Ceil", srcExpr)

  , ("CmpE", binOp)
  , ("CmpNe", binOp)
  , ("CmpSge", binOp)
  , ("CmpSgt", binOp)
  , ("CmpSle", binOp)
  , ("CmpSlt", binOp)

  , ("CmpUge", binOp)
  , ("CmpUgt", binOp)
  , ("CmpUle", binOp)
  , ("CmpUlt", binOp)

  , ("Const", constInt64)
  , ("ConstPtr", constInt64)
  , ("ConstFloat", constDouble)

  , ("DivsDp", binOp)
  , ("Divs", binOp)
  , ("DivuDp", binOp)
  , ("Divu", binOp)

  , ("Fabs", srcExpr)

  , ("Fadd", binOp)
  , ("FcmpE", binOp)
  , ("FcmpGe", binOp)
  , ("FcmpGt", binOp)
  , ("FcmpLe", binOp)
  , ("FcmpLt", binOp)
  , ("FcmpNe", binOp)
  , ("FcmpO", binOp)
  , ("FcmpUo", binOp)
  , ("Fdiv", binOp)  
  , ("FloatConv", srcExpr)
  , ("FloatToInt", srcExpr)
  , ("Floor", srcExpr)
  , ("Fmul", binOp)
  , ("Fsub", binOp)
  , ("Fneg", srcExpr)
  , ("Fsqrt", srcExpr)
  , ("Ftrunc", srcExpr)

  , ("Import", constInt64)
  , ("IntToFloat", srcExpr)

  , ("Load", srcExpr)
  , ("LowPart", srcExpr)
  
  , ("Lsl", binOp)
  , ("Lsr", binOp)

  , ("ModsDp", binOp)
  , ("ModuDp", binOp)
  , ("Mods", binOp)
  , ("Modu", binOp)
  , ("Mul", binOp)
  , ("MulsDp", binOp)
  , ("MuluDp", binOp)

  , ("Neg", srcExpr)
  
  , ("Not", srcExpr)

  , ("Or", binOp)

  , ("Popcnt", srcExpr)
  
  , ("Rlc", binOpWithCarry)
  , ("Rol", binOp)
  , ("Ror", binOp)
  
  , ("RoundToInt", srcExpr)

  , ("Rrc", binOpWithCarry)

  , ("Sbb", binOpWithCarry)
  , ("Sub", binOp)
  , ("SubWillOverflow", binOp)
  , ("Sx", srcExpr)

  , ("TestBit", binOp)
  , ("Xor", binOp)
  , ("Zx", srcExpr)
  ]
  where
    expr x = [(x, "expr")]
    leftExpr = expr "left"
    rightExpr = expr "right"
    carryExpr = expr "carry"
    srcExpr = expr "src"
    constInt64 = [("constant", "Int64")]
    constDouble = [("constant", "Double")]

    binOp = leftExpr <> rightExpr
    binOpWithCarry = binOp <> carryExpr

printOpModule :: Text -> [(Text, Text)] -> Printer ()
printOpModule name args = do
  when (length args == 1) . pr $
    "{- HLINT ignore \"Use newtype instead of data\" -}"
  pr $ "module Blaze.Types.Pil.Op." <> opName <> " where"
  br
  pr "-- This module is generated. Please use app/gen_pil_ops/Main.hs to modify."
  br
  pr "import Blaze.Prelude"
  br
  pr $ "data " <> opName <> " expr = " <> opName
  indent $ case args of
    [] -> pr derivingClause
    ((firstArgName, firstArgType):restArgs) -> do
      pr $ "{ " <> firstArgName <> " :: " <> firstArgType
      for_ restArgs $ \(argName, argType) -> do
        pr $ ", " <> argName <> " :: " <> argType
      pr $ "} " <> derivingClause
  br
  pr derivingHashable
  br
  where
    derivingClause = "deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)"
    derivingHashable = "instance Hashable a => Hashable (" <> opName <> " a)"
    opName = name <> "Op"

writeRecordModule :: FilePath -> (Text, [(Text, Text)]) -> IO ()
writeRecordModule outDir (name, args) =
  TextIO.writeFile outModulePath $ Printer.toText $ printOpModule name args
  where
    opName = name <> "Op"
    outModulePath = outDir </> recordName <.> ".hs" where
      recordName = Text.unpack opName

printLenses :: IO ()
printLenses = putText $ Printer.toText $ for_ (sort $ HashMap.keys ops) $ \x -> do
  let opName = x <> "Op"
  pr $ "$(makeFieldsNoPrefix ''" <>  opName <> ")"


printImports :: IO ()
printImports = putText $ Printer.toText $ for_ (sort $ HashMap.keys ops) $ \x -> do
  let opName = x <> "Op"
  pr $ "import Blaze.Types.Pil.Op." <> opName <> " (" <> opName <> ")"

writeRecords :: FilePath -> IO ()
writeRecords outDir = mapM_ (writeRecordModule outDir) . HashMap.toList $ ops

opDirPath :: FilePath
opDirPath = "src/Blaze/Types/Pil/Op"

opExportModulePath :: FilePath
opExportModulePath = "src/Blaze/Types/Pil/Ops.hs"

exportModulePrinter :: Printer ()
exportModulePrinter = do
  pr "module Blaze.Types.Pil.Ops (module Exports) where"
  br
  for_ (sort $ HashMap.keys ops) $ \x -> do
    let opName = x <> "Op"
    pr $ "import Blaze.Types.Pil.Op." <> opName <> " as Exports (" <> opName <> "(" <> opName <> "))"

  
writeExportModule :: IO ()
writeExportModule = TextIO.writeFile opExportModulePath . Printer.toText $ exportModulePrinter

main :: IO ()
main = do
  writeRecords opDirPath
  putText $ "Wrote op record modules to " <> cs opDirPath
  writeExportModule
  putText $ "Wrote op export module to " <> cs opExportModulePath


