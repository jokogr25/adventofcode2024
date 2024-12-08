{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day1.Day1 (part1) where

import qualified Data.List as List

part1 :: IO Int
part1 = do
  input <- readFile "src/Day1/input.txt"
  let (a, b) = unzip (map parseLine $ lines input)

  return $
    calcDistance
      ( zip
          (List.sort a)
          (List.sort b)
      )

parseLine :: String -> (Int, Int)
parseLine line =
  let [x, y] = words line
   in (read x, read y)

calcDistance :: [(Int, Int)] -> Int
calcDistance = foldl (\acc (a, b) -> acc + abs (a - b)) 0
