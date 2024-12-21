module Day3.Day3 (part1) where

import Text.Regex.TDFA

part1 :: IO Int
part1 = do
  input <- dayThreeReadFile
  print input
  print (input =~ regex :: Bool)
  mapM_ print (getAllTextMatches (input =~ regex) :: [String])
  return 0

regex :: String
regex = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"

dayThreeReadFile :: IO String
dayThreeReadFile = do
  readFile "src/Day3/example.txt"
