module A04 (a04) where

import Data.List
import Parser

testInput :: String
testInput =
  "2-4,6-8\n\
  \2-3,4-5\n\
  \5-7,7-9\n\
  \2-8,3-7\n\
  \6-6,4-6\n\
  \2-6,4-8\n"

data Sections = Sections Int Int
  deriving (Show)

data Row = Row Sections Sections
  deriving (Show)

a04 :: IO ()
a04 = do
  a041
  a042

a041 :: IO ()
a041 = do
  content <- readFile "input04"
  -- content <- pure testInput
  rows <- parseContent rowParser content
  let inRanges = map isOverlappingRange rows
      overlaps = filter id inRanges

  -- print rows
  -- print inRanges
  print $ length overlaps

a042 :: IO ()
a042 = do
  content <- readFile "input04"
  -- content <- pure testInput
  rows <- parseContent rowParser content
  let inRanges = map hasOverlapping rows
      overlaps = filter id inRanges

  print $ length overlaps

rowParser :: Parser [Row]
rowParser = many row

row :: Parser Row
row = do
  section11 <- decimal
  char '-'
  section12 <- decimal
  char ','
  section21 <- decimal
  char '-'
  section22 <- decimal
  newline
  return $ Row (Sections section11 section12) (Sections section21 section22)

isOverlappingRange :: Row -> Bool
isOverlappingRange (Row (Sections begin1 end1) (Sections begin2 end2)) =
  intersection == range1 || intersection == range2
  where
    range1 = [begin1 .. end1]
    range2 = [begin2 .. end2]
    intersection = range1 `intersect` range2

hasOverlapping :: Row -> Bool
hasOverlapping (Row (Sections begin1 end1) (Sections begin2 end2)) =
  not $ null intersection
  where
    range1 = [begin1 .. end1]
    range2 = [begin2 .. end2]
    intersection = range1 `intersect` range2
