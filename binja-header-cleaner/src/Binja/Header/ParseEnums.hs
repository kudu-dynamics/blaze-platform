{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}

module Binja.Header.ParseEnums where

import Binja.Header.Prelude hiding (option)
import Text.RawString.QQ
import Binja.Header.Types.Printer ( Printer, pr, indent )
import qualified Binja.Header.Types.Printer as Pr
import Data.Attoparsec.Text ( Parser
                            , parseOnly
                            , char
                            , string
                            , endOfInput
                            , hexadecimal
                            , anyChar
                            , satisfy
                            , letter
                            , skipSpace
                            , decimal
                            , sepBy1
                            , signed
                            , space
                            , option
                            )
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Char as Char

h :: FilePath
h = "/tmp/kudu/binaryninja-api/binaryninjacore.h"

newtype EnumName = EnumName Text
  deriving newtype (Eq, Ord, Read, Show, IsString)

newtype EnumField = EnumField Text
  deriving newtype (Eq, Ord, Read, Show, IsString)

data EnumVals = SimpleEnumVals [EnumField]
              | SpecificEnumVals [(EnumField, Int)]
              deriving (Eq, Ord, Read, Show)

data EnumType = EnumType EnumName EnumVals
  deriving (Eq, Read, Ord, Show)

hex :: (Integral a, Bits a) => Parser a
hex = string "0x" >> hexadecimal

printEnumType :: EnumType -> Printer ()
printEnumType (EnumType (EnumName name) vals) = do
  pr $ "typedef enum " <> name
  pr "{"
  indent $ case vals of
    SimpleEnumVals fs -> Pr.commaList $ (\(EnumField t) -> t) <$> fs
    SpecificEnumVals fs -> Pr.commaList $ conv <$> fs
  pr $ "} " <> name <> ";"
  where
    conv (EnumField t, n) = t <> " = " <> show n

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
  n <- signed $ hex <|> decimal
  return (ef, n)

padded_ :: Parser a -> Parser ()
padded_ p = skipSpace >> p >> skipSpace

comma :: Parser ()
comma = padded_ $ char ','

specificEnumVals :: Parser [(EnumField, Int)]
specificEnumVals = do
  vals <- sepBy1 specificEnumVal comma
  option () comma
  return vals

simpleEnumVals :: Parser [EnumField]
simpleEnumVals = do
  vals <- sepBy1 enumField comma
  option () comma
  return vals

enumVals :: Parser EnumVals
enumVals = SpecificEnumVals <$> specificEnumVals
       <|> SimpleEnumVals <$> simpleEnumVals
  
parseEnumType :: Parser EnumType
parseEnumType = do
  void $ string "enum" >> space
  skipSpace
  name <- EnumName <$> variableName
  padded_ $ char '{'
  vals <- enumVals
  padded_ $ char '}'
  void $ char ';'
  return $ EnumType name vals

demo1 :: Text
demo1 = "enum Higgins { Jack = 4, Binji = 0x32, HHHUGS = 0 };"

-- enum1 :: EnumType
-- enum1 = let (Right r) = parseOnly parseEnumType demo1 in r

-- demo2 :: Text
-- demo2 = "enum Higgins { Jack, Binji, HHHUGS };"

-- enum2 :: EnumType
-- enum2 = let (Right r) = parseOnly parseEnumType demo2 in r

-- demo3 :: Text
-- demo3 = "enum Higgins { Jack, //this has a comment\n Binji,\n //comment on its own line\n HHHUGS };"


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

    indent' = Text.replicate (Text.length dataLine - 2) " " <> "| "

    printFields :: [EnumField] -> Text
    printFields [] = ""
    printFields (x:xs) = Text.intercalate "\n" $ x':((indent' <>) <$> xs') where
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
            toEnumLine (EnumField field, n) =
              "  toEnum " <> show n <> " = " <> field
            fromEnumLine (EnumField field, n) =
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

demo2 :: Text
demo2 = [r|enum BNSegmentFlag
	{
		SegmentExecutable = 1,
		SegmentWritable = 2,
		SegmentReadable = 4,
		SegmentContainsData = 8,
		SegmentContainsCode = 10,
		SegmentDenyWrite = 20,
		SegmentDenyExecute = 11
	};
|]

