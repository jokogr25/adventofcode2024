module Main (main) where

import qualified Day1.Day1 as Day1 (day1)

main :: IO ()
main = do
  numbers <- Day1.day1
  print numbers
