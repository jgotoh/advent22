module A01 (a01) where

import Data.Foldable
import qualified Data.List as List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Read

type CaloriesEntry = Int

type Elf = [CaloriesEntry]

-- separated by blank line
type Elves = [Elf]

type Parser = Parsec Void String

a01 :: IO ()
a01 = do
  content <- readFile "input01"

  -- elves = readElves contents
  -- let testinput = "3427\n3273\n5615\n5943\n3125\n4245\n4194\n3243\n4283\n1790\n5355\n4239\n5541\n\n3850\n"
  elves <- parseContent content

  let calories = computeCalories elves
      max' = maximum calories
      sorted = reverse $ List.sort calories
      topThree = take 3 sorted
      sumTop = sum topThree

  print $ "max " <> show max'
  print $ "sum top three " <> show sumTop

parseContent :: String -> IO Elves
parseContent content = do
  let result = parse parseElves "" content
  case result of
    Left err -> do
      print $ errorBundlePretty err
      return []
    Right elves -> return elves

parseElves :: Parser Elves
parseElves = many elf

elf :: Parser Elf
elf = do
  (current :: CaloriesEntry) <- decimal
  next <- remainingCalories
  return (current : next)

remainingCalories :: Parser Elf
remainingCalories =
  do
    try (newline >> newline >> return [])
    <|> try (newline >> elf)
    <|> return []

addToElf :: CaloriesEntry -> Elf -> Elf
addToElf cal elf = elf <> [cal]

computeCalories :: Elves -> [CaloriesEntry]
computeCalories = map sum

-- parse Elves without Megaparsec
readElves :: [String] -> Elves
readElves = foldl' accum [[]]

-- if number add to previous elf, otherwise add new elf
accum :: Elves -> String -> Elves
accum elves str = case readCalories of
  Just cal -> previousElves <> [addToElf cal lastElf]
  Nothing -> elves <> [[]]
  where
    previousElves = init elves
    lastElf = last elves
    readCalories = readMaybe str :: Maybe CaloriesEntry
