{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CsvParserSpec
  ( spec,
  )
where

import Test.Hspec
import Text.Parsec.String
import Text.Parsec

data CsvFile = CsvFile CsvHeaders CsvLines deriving (Show, Eq)

newtype CsvHeaders = CsvHeaders [String] deriving (Show, Eq)

newtype CsvLines = CsvLines [String] deriving (Show, Eq)

csvFile :: Parser CsvFile
csvFile = CsvFile (CsvHeaders []) (CsvLines []) <$ eof

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "CSV Parser" $
    context "whatever" $
      it "parses empty file" $
        parseFromFile csvFile "example-files/empty.csv" `shouldReturn` Right (CsvFile (CsvHeaders []) (CsvLines []))
