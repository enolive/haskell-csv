{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CsvSpec
  ( spec,
  )
where

import Csv
import ParseFunctions
import Test.Hspec
import Text.Parsec.String

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "CSV Parser" $ do
    context "parsing a line" $ do
      it "parses single column" $
        regularParse csvLine "first" `shouldBe` Right (CsvLine ["first"])
      it "parses comma separated multiple columns" $
        regularParse csvLine "first,second,third" `shouldBe` Right (CsvLine ["first", "second", "third"])
      it "ignores whitespaces between columns" $
        regularParse csvLine "first       , second" `shouldBe` Right (CsvLine ["first", "second"])
      it "parses quoted columns" $
        regularParse csvLine "\"first\",\"second,third\"" `shouldBe` Right (CsvLine ["first", "second,third"])
    context "parsing multiple lines" $ do
      it "parses two lines" $
        regularParse csvLines "first,second\nthird,fourth" `shouldBe` Right [CsvLine ["first", "second"], CsvLine ["third", "fourth"]]
      it "parses two lines in windows" $
        regularParse csvLines "first,second\r\nthird,fourth" `shouldBe` Right [CsvLine ["first", "second"], CsvLine ["third", "fourth"]]
    context "parsing whole file" $ do
      it "parses empty file" $
        parseFromFile csvFile "example-files/empty.csv" `shouldReturn` Right (CsvFile {headers = CsvLine [], Csv.lines = [CsvLine []]})
      xit "parses file with single column" $
        parseFromFile csvFile "example-files/single-column.csv"
          `shouldReturn` Right
            ( CsvFile
                (CsvLine ["column"])
                [CsvLine ["first", "second", "third"]]
            )
