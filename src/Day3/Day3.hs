module Day3.Day3 (part1) where

import Text.Regex.TDFA

part1 :: IO Int
part1 = do
  input <- readFile "src/Day3/input.txt"
  return
    ( sum
        (map (product . f) (getAllTextMatches (input =~ "mul\\([0-9]{1,3},[0-9]{1,3}\\)") :: [String]))
    )

f :: String -> [Int]
f x = map read (getAllTextMatches (x =~ "[0-9]{1,3}") :: [String])
