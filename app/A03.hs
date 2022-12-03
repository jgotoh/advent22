module A03 (a03) where

import Data.Char (isUpper, ord, toLower)
import Data.Foldable
import Data.List
import Data.Void
import Test.Tasty.HUnit ((@?=))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import Text.Read

testInput :: String
testInput =
  "vJrwpWtwJgWrhcsFMMfFFhFp\n\
  \jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
  \PmmdzqPrVvPwwTWBwg\n\
  \wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
  \ttgJtRGJQctTZtZT\n\
  \CrZsJsPPZsGzwwsLwLmpwMDw\n"

type Rucksack = String

type Compartment = String

a03 :: IO ()
a03 = do
  a031
  a032

-- Find the item type that appears in both compartments of each rucksack.
-- What is the sum of the priorities of those item types?
a031 :: IO ()
a031 = do
  characterPrio 'a' @?= 1
  characterPrio 'Z' @?= 52
  characterPrio 'y' @?= 25

  content <- readFile "input03"
  -- content <- pure testInput
  let rucksacks = lines content
      cs = map compartments rucksacks
      as = map (uncurry intersect) cs
      prios = map (characterPrio . head) as
      result = sum prios

  print $ "result1 " <> show result

rucksackGroups :: [String] -> [[String]]
rucksackGroups rs = if null rs then [] else firstGroup : rucksackGroups (drop 3 rs)
  where
    firstGroup = take 3 rs

a032 :: IO ()
a032 = do
  content <- readFile "input03"
  let rucksacks = lines content
      groups = rucksackGroups rucksacks
      prioGroups = map prioGroup groups
      result = sum prioGroups

  print result

prioGroup :: [String] -> Int
prioGroup [a, b, c] = (characterPrio . head) prioABC
  where
    prioAB = a `intersect` b
    prioABC = prioAB `intersect` c

compartments :: Rucksack -> (Compartment, Compartment)
compartments rucksack = (compartmentA, compartmentB)
  where
    size' = length rucksack
    compartmentA = take n rucksack
    compartmentB = drop n rucksack
    n = div size' 2

lowercasePrio :: Char -> Int
lowercasePrio = subtract 96 . fromEnum

uppercasePrio :: Char -> Int
uppercasePrio = (+ 26) . lowercasePrio . toLower

characterPrio :: Char -> Int
characterPrio c = if isUpper c then uppercasePrio c else lowercasePrio c
