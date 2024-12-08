module Main (main) where

import qualified Day1.Day1 as Day1 (part1)

main :: IO ()
main = do
  distance <- Day1.part1
  print distance
  return ()
