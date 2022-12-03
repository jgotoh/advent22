module A02 (a02) where

import Data.Foldable
import Data.List
import Data.Void
import Parser
import Text.Read

data Shape = Rock | Paper | Scissors
  deriving (Show)

type OtherChoice = Shape

type MyChoice = Shape

data Round = Round OtherChoice MyChoice
  deriving (Show)

data RoundObj = RoundObj OtherChoice Outcome
  deriving (Show)

data Outcome = Lose | Draw | Win
  deriving (Show)

shapeOf :: Char -> Shape
shapeOf c = case c of
  'A' -> Rock
  'X' -> Rock
  'B' -> Paper
  'Y' -> Paper
  'C' -> Scissors
  'Z' -> Scissors

objOf :: Char -> Outcome
objOf c = case c of
  'X' -> Lose
  'Y' -> Draw
  'Z' -> Win

shapeScore :: Shape -> Int
shapeScore shape = case shape of
  Rock -> 1
  Paper -> 2
  Scissors -> 3

outcomeScore :: Outcome -> Int
outcomeScore outcome = case outcome of
  Lose -> 0
  Draw -> 3
  Win -> 6

a02 :: IO ()
a02 = do
  content <- readFile "input02"
  let testinput = "A Y\nB X\nC Z\n"
  rounds <- parseContent roundsParser content
  roundObjs <- parseContent roundObjParser content

  let (scores :: [Int]) = map (computeScore . computeOutcome) rounds
      (scoresObj :: [Int]) = map (computeScore . shapeForRoundObj) roundObjs
      totalScore = sum scores
      totalScore' = sum scoresObj

  print $ "totalScore " <> show totalScore
  print $ "totalScore' " <> show totalScore'

roundsParser :: Parser [Round]
roundsParser = many round'

roundObjParser :: Parser [RoundObj]
roundObjParser = many roundObj

round' :: Parser Round
round' = do
  other <- shapeOf <$> letterChar
  space
  me <- shapeOf <$> letterChar
  newline
  return $ Round other me

roundObj :: Parser RoundObj
roundObj = do
  other <- shapeOf <$> letterChar
  space
  obj <- objOf <$> letterChar
  newline
  return $ RoundObj other obj

computeOutcome :: Round -> (Outcome, Shape)
computeOutcome (Round other my) =
  let otherScore = shapeScore other
      myScore = shapeScore my
      outcome = getOutcome other my
   in (outcome, my)

shapeForRoundObj :: RoundObj -> (Outcome, Shape)
shapeForRoundObj (RoundObj other obj) = (obj, shapeByObjective other obj)

shapeByObjective :: OtherChoice -> Outcome -> MyChoice
shapeByObjective other obj = case obj of
  Lose -> case other of
    Rock -> Scissors
    Paper -> Rock
    Scissors -> Paper
  Draw -> case other of
    Rock -> Rock
    Paper -> Paper
    Scissors -> Scissors
  Win -> case other of
    Rock -> Paper
    Paper -> Scissors
    Scissors -> Rock

getOutcome :: OtherChoice -> MyChoice -> Outcome
getOutcome other my = case other of
  Rock -> case my of
    Rock -> Draw
    Paper -> Win
    Scissors -> Lose
  Paper -> case my of
    Rock -> Lose
    Paper -> Draw
    Scissors -> Win
  Scissors -> case my of
    Rock -> Win
    Paper -> Lose
    Scissors -> Draw

computeScore :: (Outcome, Shape) -> Int
computeScore (outcome, shape) = outcomeScore' + shapeScore'
  where
    outcomeScore' = outcomeScore outcome
    shapeScore' = shapeScore shape
