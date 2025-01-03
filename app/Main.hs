module Main (main) where

import qualified Day1.Day1 as Day1 (part1, part2)
import qualified Day2.Day2 as Day2 (part1, part2)
import qualified Day3.Day3 as Day3 (part1, part2)

main :: IO ()
main = do
  day1part1 <- Day1.part1
  print ("Day1 - Part 1: " ++ show day1part1)

  day1part2 <- Day1.part2
  print ("Day1 - Part 2: " ++ show day1part2)

  day2part1 <- Day2.part1
  print ("Day2 - Part 1: " ++ show day2part1)

  day2part2 <- Day2.part2
  print ("Day2 - Part 2: " ++ show day2part2)

  day3part1 <- Day3.part1
  print ("Day3 - Part 1: " ++ show day3part1)

  day3part2 <- Day3.part2
  print ("Day3 - Part 2: " ++ show day3part2)

  return ()
