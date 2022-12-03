module A01 (a01) where

import Data.Foldable
import Data.List
import Parser
import Text.Read

type CaloriesEntry = Int

type Elf = [CaloriesEntry]

-- separated by blank line
type Elves = [Elf]

a01 :: IO ()
a01 = do
  content <- readFile "input01"

  -- elves = readElves contents
  -- let testinput = "3427\n3273\n5615\n5943\n3125\n4245\n4194\n3243\n4283\n1790\n5355\n4239\n5541\n\n3850\n"
  elves <- parseContent parseElves content

  let calories = computeCalories elves
      max' = maximum calories
      sorted = reverse $ sort calories
      topThree = take 3 sorted
      sumTop = sum topThree

  print $ "max " <> show max'
  print $ "sum top three " <> show sumTop

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
