{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day1.Day1 (day1) where

import qualified Data.List as List
import qualified Data.Set as Set

day1 :: IO Int
day1 = do
  input <- readFile "src/Day1/test.txt"
  let numbers = map parseLine $ lines input
  let (a, b) = unzip numbers
  return $ calcDistance (zip (List.sort $ Set.toList $ Set.fromList a) (List.sort $ Set.toList $ Set.fromList b))

parseLine :: String -> (Int, Int)
parseLine line =
  let [x, y] = words line
   in (read x, read y)

calcDistance :: [(Int, Int)] -> Int
calcDistance = foldl (\acc (a, b) -> acc + abs (a - b)) 0