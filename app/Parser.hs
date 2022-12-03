module Parser
  ( module Text.Megaparsec,
    module Text.Megaparsec.Char,
    module Text.Megaparsec.Char.Lexer,
    Parser,
    parseContent,
  )
where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser = Parsec Void String

parseContent :: Parser a -> String -> IO a
parseContent parser content = do
  let result = parse parser "" content
  case result of
    Left err -> do
      error $ errorBundlePretty err
    Right xs -> return xs
