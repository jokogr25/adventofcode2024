{-# OPTIONS_GHC -Wno-unused-matches #-}

module Day2.Day2 (part1) where

data Direction = Up | Down | Init | None deriving (Show, Eq)

part1 :: IO Int
part1 = do
  input <- dayTwoReadFile
  print input
  print (map (isSafe Init) input)
  length . filter id . map (isSafe Init) <$> dayTwoReadFile

isSafe :: Direction -> [Int] -> Bool
isSafe _ [] = False
isSafe _ [_] = True
isSafe dir (x : xs : xss)
  | dir == Init = isSafe (checkDirection x xs) (x : xs : xss)
  | dir == None = False
  | (abs (x - xs) >= 1 && abs (x - xs) <= 3) && checkDirection x xs == dir = isSafe (checkDirection x xs) (xs : xss)
  | otherwise = False

checkDirection :: Int -> Int -> Direction
checkDirection x y
  | x < y = Up
  | x > y = Down
  | otherwise = None

dayTwoReadFile :: IO [[Int]]
dayTwoReadFile = do
  input <- readFile "src/Day2/input.txt"
  return $ map parseLine (lines input)

parseLine :: String -> [Int]
parseLine line = map read $ words line
