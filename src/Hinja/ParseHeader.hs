{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Hinja.ParseHeader where

import Hinja.Prelude
import Hinja.Types.Printer ( Printer, pr, indent )
import qualified Hinja.Types.Printer as Pr
import Language.C.System.GCC (newGCC)
import Language.C (parseCFile)
import Language.C.Syntax.AST (CTranslUnit)
import Language.C.Parser (ParseError)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.IO (openTempFile, hClose)
import Hinja.ParseStruct (parseStruct, Struct, printStruct)
import Hinja.ParseFunction (parseFunction, Function, printFunction)
import Hinja.ParseEnums (parseEnumType, EnumType, findAll, printEnumType)
import           Data.Attoparsec.Text                  ( Parser
                                                       , anyChar
                                                       , char
                                                       , decimal
                                                       , endOfInput
                                                       , letter
                                                       , manyTill
                                                       , many1
                                                       , parseOnly
                                                       , satisfy
                                                       , sepBy1
                                                       , skipSpace
                                                       , space
                                                       , string
                                                       , takeTill
                                                       , takeWhile
                                                       )

data Decl = DeclStruct Struct
          | DeclFunction Function
          | DeclEnum EnumType
          deriving (Eq, Ord, Read, Show)

parseDecl :: Parser Decl
parseDecl = (DeclStruct <$> parseStruct)
  <|> (DeclFunction <$> parseFunction)
  <|> (DeclEnum <$> parseEnumType)

parseAllDecls :: Parser [Decl]
parseAllDecls = findAll parseDecl

getDecls :: Text -> Either Text [Decl]
getDecls = first Text.pack . parseOnly parseAllDecls

printDecl :: Decl -> Printer ()
printDecl (DeclStruct s) = printStruct s
printDecl (DeclFunction f) = printFunction f
printDecl (DeclEnum et) = printEnumType et

printAllDecls :: [Decl] -> Printer ()
printAllDecls = indent . mapM_ f
  where
    f d = do
      printDecl d
      pr ""

kosherHeader :: Text -> Text
kosherHeader = Pr.toText . printAllDecls . either (const []) identity . getDecls

writeKosherHeader :: FilePath -> FilePath -> IO ()
writeKosherHeader fpin fpout = do
  hin <- TextIO.readFile fpin
  TextIO.writeFile fpout $ kosherHeader hin
  

demo :: IO (Either ParseError CTranslUnit) 
demo = do
  let gcc = newGCC "gcc"
  htext <- cleanHeader <$> TextIO.readFile "/tmp/h/binaryninjacore.h"
  (tempFp, h) <- openTempFile "/tmp" "binaryninjacore.h"
  hClose h
  TextIO.writeFile tempFp htext
  parseCFile gcc Nothing [] tempFp

commentOut :: Text -> Text -> Text
commentOut needle =
  Text.replace needle (" // " <> needle)

cleanHeader :: Text -> Text
cleanHeader = commentOut "typedef bool (*BNCorePluginInitFunction)(void);"
  . commentOut "typedef bool (*BNLoadPluginCallback)(const char* repoPath, const char* pluginPath, void* ctx);"
