module A05 where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as ST
import Debug.Trace
import Parser

testInput :: String
testInput =
  "    [D]    \n\
  \[N] [C]    \n\
  \[Z] [M] [P]\n\
  \ 1   2   3 \n\
  \n\
  \move 1 from 2 to 1\n\
  \move 3 from 1 to 3\n\
  \move 2 from 2 to 1\n\
  \move 1 from 1 to 2\n"

type StackRow = [Maybe Char]

meow :: [Maybe a] -> [a]
meow = catMaybes

stackRowsToStacks :: [StackRow] -> [Stack]
stackRowsToStacks = map meow . rotate . hackyStuff . head

hackyStuff :: [a] -> [[a]]
hackyStuff xs = current : next
  where
    current = take 9 xs
    next = if null xs then [[]] else hackyStuff (drop 9 xs)

rotate :: [[x]] -> [[x]]
rotate = transpose

type Stack = [Char]

-- Move count from x to y
data Move = Move Int Int Int
  deriving (Show)

data Cargo = Cargo [Stack] [Move]
  deriving (Show)

cargoParser :: Parser Cargo
cargoParser = do
  stacks <- stacksParser
  Cargo stacks <$> movesParser

stacksParser :: Parser [Stack]
stacksParser = do
  rows <- many stackRowParser
  takeWhileP Nothing (/= '\n')
  newline
  newline
  return $ stackRowsToStacks (trace (show rows) rows)

stackRowParser :: Parser StackRow
stackRowParser = do
  (current :: Maybe Char) <- stackEntryParser
  next <- remainingStackEntries
  return (current : next)

stackEntryParser :: Parser (Maybe Char)
stackEntryParser =
  do
    try (spaceChar >> spaceChar >> spaceChar >> spaceChar >> return Nothing)
    <|> ( do
            char '['
            c <- letterChar
            char ']'
            spaceChar
            return $ Just c
        )

remainingStackEntries :: Parser StackRow
remainingStackEntries = do
  stackRowParser <|> return []

movesParser :: Parser [Move]
movesParser = do
  current <- moveParser
  next <- remainingMoves
  return (current : next)

moveParser :: Parser Move
moveParser = do
  string "move"
  space
  count <- decimal
  space
  string "from"
  space
  from <- decimal
  space
  string "to"
  space
  Move count from <$> decimal

remainingMoves :: Parser [Move]
remainingMoves =
  do
    try (newline >> movesParser)
    <|> return []

a05 :: IO ()
a05 = do
  a051

a051 :: IO ()
a051 = do
  content <- readFile "input05"
  cargo@(Cargo oldStacks allMoves) <- parseContent cargoParser content
  let (xs :: [[Int]]) =
        [ [1, 4, 7],
          [2, 5, 8],
          [3, 6, 9]
        ]
  print $ rotate xs
  let finalCargo = foldl' applyMove oldStacks allMoves
      heads' = map head finalCargo
  print heads'

-- change to newTo = '(reverse moving)' <> oldTo for first puzzle
applyMoveVector :: V.Vector Stack -> Move -> V.Vector Stack
applyMoveVector stacks (Move num from to) = vMoved
  where
    oldFrom = (V.!) stacks (from - 1)
    oldTo = (V.!) stacks (to - 1)
    (moving, newFrom) = splitAt num oldFrom
    newTo = moving <> oldTo
    vDropped = V.modify (\v -> ST.write v (from - 1) newFrom) stacks
    vMoved = V.modify (\v -> ST.write v (to - 1) newTo) vDropped

applyMove :: [Stack] -> Move -> [Stack]
applyMove stack move = outStacks
  where
    inVec = V.fromList stack
    outVec = applyMoveVector inVec move
    outStacks = V.toList outVec
