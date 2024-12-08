module Main (main) where

import qualified Day1.Day1 as Day1 (day1)

main :: IO ()
main = do
  distance <- Day1.day1
  print distance
  return ()