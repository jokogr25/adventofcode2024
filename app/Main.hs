module Main (main) where

import qualified Day1.Day1 as Day1 (part1, part2)

main :: IO ()
main = do
  distance <- Day1.part1
  sum <- Day1.part2
  print distance
  print sum
  return ()
