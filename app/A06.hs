module A06 (a06) where

import Data.List
import Data.Maybe

a06 :: IO ()
a06 = do
  content <- readFile "input06"
  let line = (head . lines) content
      marker = findMarker line 4
      messageMarker = findMarker line 14
  print marker
  print messageMarker

findMarker :: String -> Int -> Int
findMarker str len = i + len
  where
    subs = substrings len str
    sub = findSubStr subs
    i = fromJust $ elemIndex sub subs

findSubStr :: [String] -> String
findSubStr [] = error "not found"
findSubStr (x : xs) = if nub x == x then x else findSubStr xs

substrings :: Int -> String -> [String]
substrings n s
  | length s >= n = take n s : substrings n (tail s)
  | otherwise = []
