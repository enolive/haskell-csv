{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CsvSpec
  ( spec,
  )
where

import Csv
import qualified Data.Map as Map
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
      it "parses quoted columns" $
        regularParse csvLine "\"first\",\"second,third\"" `shouldBe` Right (CsvLine ["first", "second,third"])
    context "parsing multiple lines" $ do
      it "parses single line" $
        regularParse csvLines "only line" `shouldBe` Right [CsvLine ["only line"]]
      it "parses two lines" $
        regularParse csvLines "first,second\nthird,fourth" `shouldBe` Right [CsvLine ["first", "second"], CsvLine ["third", "fourth"]]
      it "ignores empty trailing line" $
        regularParse csvLines "first\nsecond\n" `shouldBe` Right [CsvLine ["first"], CsvLine ["second"]]
      it "parses two lines in windows" $
        regularParse csvLines "first,second\r\nthird,fourth" `shouldBe` Right [CsvLine ["first", "second"], CsvLine ["third", "fourth"]]
    context "building content map" $ do
      it "works" $
        buildAll (CsvLine ["column1", "column2"]) [CsvLine ["first1", "first2"], CsvLine ["second1", "second2"]]
          `shouldBe` [ Map.fromList [("column1", "first1"), ("column2", "first2")],
                       Map.fromList [("column1", "second1"), ("column2", "second2")]
                     ]
      it "ignores content without column" $
        buildAll (CsvLine ["column1"]) [CsvLine ["first1", "first2"]]
          `shouldBe` [Map.fromList [("column1", "first1")]]
      it "fills columns without content" $
        buildAll (CsvLine ["column1", "column2"]) [CsvLine ["first1"]]
          `shouldBe` [Map.fromList [("column1", "first1"), ("column2", "")]]
    context "parsing whole file" $ do
      it "parses empty file" $
        parseFromFile csvFile "example-files/empty.csv" `shouldReturn` Right (CsvFile {headers = CsvLine [], content = []})
      it "parses file with single column" $
        parseFromFile csvFile "example-files/single-column.csv"
          `shouldReturn` Right
            ( CsvFile
                { headers = CsvLine ["column"],
                  content =
                    [ Map.fromList [("column", "first")],
                      Map.fromList [("column", "second")],
                      Map.fromList [("column", "third")]
                    ]
                }
            )
      it "parses file with multiple columns" $
        parseFromFile csvFile "example-files/multiple-columns.csv"
          `shouldReturn` Right
            ( CsvFile
                { headers = CsvLine ["column1", "column2"],
                  content =
                    [ Map.fromList [("column1", "first1"), ("column2", "first2")],
                      Map.fromList [("column1", "second1"), ("column2", "second2")],
                      Map.fromList [("column1", "third1"), ("column2", "third2")]
                    ]
                }
            )
      it "parses file with uneven structure" $
        parseFromFile csvFile "example-files/uneven.csv"
          `shouldReturn` Right
            ( CsvFile
                { headers = CsvLine ["first", "second"],
                  content =
                    [ Map.fromList [("first", "1"), ("second", "2")],
                      Map.fromList [("first", "1"), ("second", "")]
                    ]
                }
            )
