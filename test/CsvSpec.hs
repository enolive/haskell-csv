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
    context "parsing lines" $ do
      it "parses single column" $
        regularParse csvLine "first" `shouldBe` Right (CsvLine ["first"])
      it "parses comma separated multiple columns" $
        regularParse csvLine "first,second,third" `shouldBe` Right (CsvLine ["first", "second", "third"])
      it "ignores whitespaces between columns" $
        regularParse csvLine "first       , second" `shouldBe` Right (CsvLine ["first", "second"])
      it "parses quoted columns" $
        regularParse csvLine "\"first\",\"second,third\"" `shouldBe` Right (CsvLine ["first", "second,third"])
    context "parsing whole file" $ do
      it "parses empty file" $
        parseFromFile csvFile "example-files/empty.csv" `shouldReturn` Right (CsvFile (CsvLine []) [CsvLine []])
      xit "parses file with single column" $
        parseFromFile csvFile "example-files/single-column.csv"
          `shouldReturn` Right
            ( CsvFile
                (CsvLine ["column"])
                [CsvLine ["first", "second", "third"]]
            )
