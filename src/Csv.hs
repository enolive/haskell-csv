module Csv where

import Text.Parsec
import Text.Parsec.String (Parser)

data CsvFile = CsvFile CsvHeaders CsvLines deriving (Show, Eq)

newtype CsvHeaders = CsvHeaders [String] deriving (Show, Eq)

newtype CsvLines = CsvLines [String] deriving (Show, Eq)

csvFile :: Parser CsvFile
csvFile = f <$ many anyChar <* eof
  where
    f = CsvFile (CsvHeaders []) (CsvLines [])

csvHeaders :: Parser CsvHeaders
csvHeaders = f <$> many anyChar
  where
    f xs = CsvHeaders [xs]

csvLines :: Parser CsvLines
csvLines = CsvLines [] <$ sepBy (many1 anyChar) endOfLine