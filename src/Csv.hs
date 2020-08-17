module Csv where

import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token

data CsvFile = CsvFile
  { headers :: CsvHeaders,
    lines :: [CsvLine]
  }
  deriving (Show, Eq)

type CsvHeaders = CsvLine

newtype CsvLine = CsvLine [String] deriving (Show, Eq)

csvFile :: Parser CsvFile
csvFile = f <$ many anyChar <* eof
  where
    f = CsvFile (CsvLine []) [CsvLine []]

csvLine :: Parser CsvLine
csvLine = CsvLine <$> sepBy csvColumn (try (spaces *> char ',' *> spaces))

csvLines :: Parser [CsvLine]
csvLines = sepBy csvLine endOfLine

csvColumn :: Parser String
csvColumn = stringLiteral <|> many (noneOf " ,\r\n")
--csvColumn = stringLiteral <|> many alphaNum

tokenParser :: Token.GenTokenParser String u Identity
tokenParser = Token.makeTokenParser emptyDef

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral tokenParser
