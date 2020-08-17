module ParseFunctions where

import Text.Parsec.String (Parser)
import Text.Parsec (ParseError, parse, manyTill, anyToken, eof)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseWithLeftOver :: Parser a -> String -> Either ParseError (a, String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
  where
    leftOver = manyTill anyToken eof
