{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day1.Day1 (part1, part2) where

import qualified Data.List as List

part1 :: IO Int
part1 = do
  (a, b) <- dayOneReadFile

  return $
    calcDistance
      ( zip
          (List.sort a)
          (List.sort b)
      )

calcDistance :: [(Int, Int)] -> Int
calcDistance = foldl (\acc (a, b) -> acc + abs (a - b)) 0

part2 :: IO Int
part2 = do
  (a, b) <- dayOneReadFile

  return (foldl (\acc x -> acc + length (filter (== x) b) * x) 0 a)

dayOneReadFile :: IO ([Int], [Int])
dayOneReadFile = do
  input <- readFile "src/Day1/input.txt"
  return $ unzip (map parseLine $ lines input)

parseLine :: String -> (Int, Int)
parseLine line =
  let [x, y] = words line
   in (read x, read y)
