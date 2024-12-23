module Day3.Day3 (part1, part2) where

import Text.Regex.TDFA

data Dontnod = Do | Dont deriving (Show, Eq)

part1 :: IO Int
part1 = do
  input <- readFile "src/Day3/input.txt"
  return
    ( sum
        (map (product . f) (getAllTextMatches (input =~ "mul\\([0-9]{1,3},[0-9]{1,3}\\)") :: [String]))
    )

part2 :: IO Int
part2 = do
  input <- readFile "src/Day3/input.part2.txt"
  return
    ( sum
        (map (product . f) (checkCheck (getAllTextMatches (input =~ regexPart2) :: [String])))
    )

checkCheck :: [String] -> [String]
checkCheck = check Do
  where
    check :: Dontnod -> [String] -> [String]
    check _ [] = []
    check dontnod (x : xs)
      | x == "do()" = check Do xs
      | x == "don't()" = check Dont xs
      | otherwise =
          if dontnod == Do
            then x : check Do xs
            else check Dont xs

regexPart2 :: String
regexPart2 = "mul\\([0-9]{1,3},[0-9]{1,3}\\)|do\\(\\)|don't\\(\\)"

f :: String -> [Int]
f x = map read (getAllTextMatches (x =~ "[0-9]{1,3}") :: [String])
