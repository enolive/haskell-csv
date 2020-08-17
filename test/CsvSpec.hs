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
    context "parsing headers" $ do
      it "parses single column" $
        regularParse csvHeaders "column" `shouldBe` Right (CsvHeaders ["column"])
      it "parses comma separated multiple columns" $
        regularParse csvHeaders "first,second" `shouldBe` Right (CsvHeaders ["first", "second"])
      it "ignores whitespaces between columns" $
        regularParse csvHeaders "first       , second" `shouldBe` Right (CsvHeaders ["first", "second"])
      it "parses quoted columns" $
        regularParse csvHeaders "\"first\",\"second,third\"" `shouldBe` Right (CsvHeaders ["first", "second,third"])
    context "parsing whole file" $ do
      it "parses empty file" $
        parseFromFile csvFile "example-files/empty.csv" `shouldReturn` Right (CsvFile (CsvHeaders []) (CsvLines []))
      xit "parses file with single column" $
        parseFromFile csvFile "example-files/single-column.csv"
          `shouldReturn` Right
            ( CsvFile
                (CsvHeaders ["column"])
                (CsvLines ["first", "second", "third"])
            )
