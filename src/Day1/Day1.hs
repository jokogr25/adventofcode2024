{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day1.Day1 (readInput) where

readInput :: IO [(Int, Int)]
readInput = do
  input <- readFile "src/Day1/input.txt"
  let numbers = map parseLine $ lines input
  return numbers

parseLine :: String -> (Int, Int)
parseLine line =
  let [x, y] = words line
   in (read x, read y)