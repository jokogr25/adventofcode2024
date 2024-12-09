{-# OPTIONS_GHC -Wno-unused-matches #-}

module Day2.Day2 (part1, part2) where

data Direction = Up | Down | Init | None deriving (Show, Eq)

data Direction' = Up' | Down' deriving (Show, Eq)

part1 :: IO Int
part1 = do
  input <- dayTwoReadFile
  length . filter id . map (isSafe Init) <$> dayTwoReadFile

part2 :: IO Int
part2 = do
  input <- dayTwoReadFile
  length . filter id . map (isSafe2' []) <$> dayTwoReadFile

isSafe :: Direction -> [Int] -> Bool
isSafe _ [] = False
isSafe _ [_] = True
isSafe dir (x : xs : xss)
  | dir == Init = isSafe (checkDirection x xs) (x : xs : xss)
  | dir == None = False
  | (abs (x - xs) >= 1 && abs (x - xs) <= 3) && checkDirection x xs == dir = isSafe (checkDirection x xs) (xs : xss)
  | otherwise = False

-- (x : xs) von der zweiten Liste x ist der erste Wert von der dieser
isSafe2' :: [Int] -> [Int] -> Bool
isSafe2' _ [] = False
isSafe2' x (y : ys) = isSafe2' (x ++ [y]) ys || isSafe Init (x ++ ys)

checkDirection :: Int -> Int -> Direction
checkDirection x y
  | x < y = Up
  | x > y = Down
  | otherwise = None

dayTwoReadFile :: IO [[Int]]
dayTwoReadFile = do
  input <- readFile "src/Day2/example.txt"
  return $ map parseLine (lines input)

parseLine :: String -> [Int]
parseLine line = map read $ words line
