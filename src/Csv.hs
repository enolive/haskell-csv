module Csv where

import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token
import Data.Map (Map)
import qualified Data.Map as Map

data CsvFile = CsvFile
  { headers :: CsvHeaders,
    content :: [CsvColumns]
  }
  deriving (Show, Eq)

type CsvHeaders = CsvLine
type CsvColumns = Map String String

newtype CsvLine = CsvLine [String] deriving (Show, Eq)

csvFile :: Parser CsvFile
csvFile = f <$> csvLines <* spaces <* eof
  where
    f [] = CsvFile {headers = CsvLine [], content = []}
    f (x:xs) = CsvFile {headers = x, content = buildAll x xs}

buildAll :: CsvHeaders -> [CsvLine] -> [CsvColumns]
buildAll h ls = buildOne h <$> ls

buildOne :: CsvHeaders -> CsvLine -> CsvColumns
buildOne (CsvLine hs) (CsvLine cols) = foldl (tryInsert hs) initialMap (zip cols [0..])
  where
    initialMap = Map.fromList (zip hs (repeat ""))

tryInsert :: [String] -> CsvColumns -> (String, Int) -> CsvColumns
tryInsert hs cols (col,index)
  | length hs > index = Map.insert (hs !! index) col cols
  | otherwise = cols

csvLines :: Parser [CsvLine]
csvLines = removeTrailing <$> sepBy csvLine endOfLine
  where
    removeTrailing [] = []
    removeTrailing [CsvLine [""]] = []
    removeTrailing (x:xs) = x : removeTrailing xs

csvLine :: Parser CsvLine
csvLine = CsvLine <$> sepBy csvColumn (char ',')

csvColumn :: Parser String
csvColumn = stringLiteral <|> many (noneOf ",\r\n")

tokenParser :: Token.GenTokenParser String u Identity
tokenParser = Token.makeTokenParser emptyDef

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral tokenParser
