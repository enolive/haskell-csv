module Csv where

import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token

data CsvFile = CsvFile CsvHeaders CsvLines deriving (Show, Eq)

newtype CsvHeaders = CsvHeaders [String] deriving (Show, Eq)

newtype CsvLines = CsvLines [String] deriving (Show, Eq)

csvFile :: Parser CsvFile
csvFile = f <$ many anyChar <* eof
  where
    f = CsvFile (CsvHeaders []) (CsvLines [])

csvHeaders :: Parser CsvHeaders
csvHeaders = f <$> sepBy csvHeader (spaces *> char ',' *> spaces)
  where
    f hs = CsvHeaders hs
    csvHeader = stringLiteral <|> many (noneOf " ,")

csvLines :: Parser CsvLines
csvLines = CsvLines [] <$ sepBy (many1 anyChar) endOfLine

tokenParser :: Token.GenTokenParser String u Identity
tokenParser = Token.makeTokenParser emptyDef

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral tokenParser
