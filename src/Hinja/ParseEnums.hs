{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Hinja.ParseEnums where

import Hinja.Prelude 
import qualified Prelude as P
import Hinja.Types.Printer ( Printer, pr, indent )
import qualified Hinja.Types.Printer as Pr
import Data.Attoparsec.Text ( Parser
                            , parseOnly
                            , char
                            , string
                            , endOfInput
                            , anyChar
                            , satisfy
                            , letter
                            , skipSpace
                            , takeWhile
                            , decimal
                            , sepBy1
                            , space
                            )
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Char (isUpper, isAlphaNum)
import qualified Data.Char as Char

h :: FilePath
h = "/tmp/kudu/binaryninja-api/binaryninjacore.h"

newtype EnumName = EnumName Text
  deriving (Eq, Ord, Read, Show)

newtype EnumField = EnumField Text
  deriving (Eq, Ord, Read, Show)

data EnumVals = SimpleEnumVals [EnumField]
              | SpecificEnumVals [(EnumField, Int)]
              deriving (Eq, Ord, Read, Show)

data EnumType = EnumType EnumName EnumVals
  deriving (Eq, Read, Ord, Show)

printEnumType :: EnumType -> Printer ()
printEnumType (EnumType (EnumName name) vals) = do
  pr $ "typedef enum " <> name
  pr $ "{"
  indent $ case vals of
    SimpleEnumVals fs -> Pr.commaList $ (\(EnumField t) -> t) <$> fs
    SpecificEnumVals fs -> Pr.commaList $ conv <$> fs
  pr $ "} " <> name <> ";"
  where
    conv ((EnumField t), n) = t <> " = " <> show n

variableNameChar :: Parser Char
variableNameChar = satisfy p where
  p c = isAlphaNum c || c == '_'

variableName :: Parser Text
variableName = do
  l <- letter
  rest <- many variableNameChar
  return . Text.pack $ l:rest
  
enumField :: Parser EnumField
enumField = EnumField <$> p where
  p = do
    l <- letter
    rest <- many variableNameChar
    return . Text.pack $ Char.toUpper l : rest


specificEnumVal :: Parser (EnumField, Int)
specificEnumVal = do
  ef <- enumField
  skipSpace
  void $ char '='
  skipSpace
  n <- decimal
  return (ef, n)

padded_ :: Parser a -> Parser ()
padded_ p = skipSpace >> p >> skipSpace

comma :: Parser ()
comma = padded_ $ char ','

specificEnumVals :: Parser [(EnumField, Int)]
specificEnumVals = sepBy1 specificEnumVal comma

simpleEnumVals :: Parser [EnumField]
simpleEnumVals = sepBy1 enumField comma

enumVals :: Parser EnumVals
enumVals = SpecificEnumVals <$> specificEnumVals
       <|> SimpleEnumVals <$> simpleEnumVals
  
parseEnumType :: Parser EnumType
parseEnumType = do
  void $ string "enum"
  skipSpace
  name <- EnumName <$> variableName
  padded_ $ char '{'
  vals <- enumVals
  padded_ $ char '}'
  void $ char ';'
  return $ EnumType name vals

demo1 :: Text
demo1 = "enum Higgins { Jack = 4, Binji = 32, HHHUGS = 0 };"

enum1 :: EnumType
enum1 = let (Right r) = parseOnly parseEnumType demo1 in r

demo2 :: Text
demo2 = "enum Higgins { Jack, Binji, HHHUGS };"

enum2 :: EnumType
enum2 = let (Right r) = parseOnly parseEnumType demo2 in r

demo3 :: Text
demo3 = "enum Higgins { Jack, //this has a comment\n Binji,\n //comment on its own line\n HHHUGS };"


findAll :: Parser a -> Parser [a]
findAll p = end <|> found <|> continue where
  end = endOfInput >> return []
  found = do
    x <- p
    (x:) <$> findAll p
  continue = anyChar >> findAll p


parseEnums :: FilePath -> IO [EnumType]
parseEnums = fmap (either (const []) identity) . parseEnums'

parseEnums' :: FilePath -> IO (Either Text [EnumType])
parseEnums' fp = do
  t <- TextIO.readFile fp
  return . first Text.pack $ parseOnly (findAll parseEnumType) t


-----------------------------------

enumToModule :: Text -> EnumType -> Text
enumToModule modulePrefix (EnumType (EnumName name) vals) = header <> enum vals
  where
    header = "module " <> modulePrefix <> "." <> name <> " where\n\n"

    dataLine = "data " <> name <> " = "

    indent = Text.replicate (Text.length dataLine - 2) " " <> "| "

    printFields :: [EnumField] -> Text
    printFields [] = ""
    printFields (x:xs) = Text.intercalate "\n" $ x':((indent <>) <$> xs') where
      xs' = enumFieldToText <$> xs
      x' = enumFieldToText x

    enumFieldToText :: EnumField -> Text
    enumFieldToText (EnumField t) = t

    enum (SimpleEnumVals fields) =
      dataLine <> printFields fields <> derive
      where
        derive = "\n  deriving (Enum, Eq, Ord, Read, Show)"
    enum (SpecificEnumVals specs) = 
      dataLine <> printFields (fst <$> specs) <> derive <> enumInstance
      where
        derive = "\n  deriving (Eq, Ord, Read, Show)"
        enumInstance = "\n\n"
          <> "instance Enum " <> name <> " where\n"
          <> Text.concat (toEnumLine <$> specs)
          <> Text.concat (fromEnumLine <$> specs)
          where
            toEnumLine ((EnumField field), n) =
              "  toEnum " <> show n <> " = " <> field
            fromEnumLine ((EnumField field), n) =
              "  fromEnum " <> field <> " = " <> show n

writeEnumModule :: Text -> FilePath -> EnumType -> IO ()
writeEnumModule modulePrefix enumDir enum@(EnumType (EnumName name) _) =
  TextIO.writeFile (enumDir <> Text.unpack name <> ".hs")
  $ enumToModule modulePrefix enum <> "\n"

writeEnumModules :: Text -> FilePath -> [EnumType] -> IO ()
writeEnumModules modulePrefix enumDir = mapM_ $ writeEnumModule modulePrefix enumDir

-- parseAndWriteEnumModules :: Text -> FilePath -> FilePath -> IO [EnumType]
-- parseAndWriteEnumModules modulePrefix enumDir cHeaderFile = do
--   enums <- parseEnums cHeaderFile
--   writeEnumModules modulePrefix enumDir enums
--   return enums
--   where
--     enumDir' = maybe "" (bool (enumDir `Text.snoc` '/') enumDir $ Text.last == "/")
--       $ lastMay enumDir

