{-# OPTIONS_GHC -Wno-unused-matches #-}

module Day2.Day2 (part1, part2) where

data Direction = Up | Down | Plateau deriving (Show, Eq)

part1 :: IO Int
part1 = do
  input <- dayTwoReadFile
  length . filter id . map isSafe <$> dayTwoReadFile

part2 :: IO Int
part2 = do
  input <- dayTwoReadFile
  length . filter id . map isSafe2 <$> dayTwoReadFile

isSafe :: [Int] -> Bool
isSafe [] = False
isSafe [_] = False
isSafe [x, xs] = isSafeIncDec x xs
isSafe (x : xs : xss) = isSafe' (checkDirection x xs) (x : xs : xss)
  where
    isSafe' :: Direction -> [Int] -> Bool
    isSafe' _ [] = False
    isSafe' _ [_] = True
    isSafe' dir (y : ys : yss)
      | isSafeIncDec y ys && checkDirection y ys == dir = isSafe' dir (ys : yss)
      | otherwise = False

isSafeIncDec :: Int -> Int -> Bool
isSafeIncDec x y = abs (x - y) >= 1 && abs (x - y) <= 3

isSafe'' :: [Int] -> Bool
isSafe'' [] = False
isSafe'' [x, xs] = isSafeIncDec x xs
isSafe'' xs =
  and (f isSafeInc) || and (f isSafeDec)
  where
    f g = foldl (\a (b, bs) -> a ++ [g b bs]) [] (zip xs (tail xs))

    isSafeInc x y = (y - x) >= 1 && (y - x) <= 3

    isSafeDec x y = (x - y) >= 1 && (x - y) <= 3

{-
der head der zweiten Liste ist der Wert, der in jedem Durchlauf rausgenommen wird
in jedem "Durchlauf" wird ein Element entfernt und die Liste ohne dieses Element geprüft
der "Trick" ist nun: wenn ein Element entfernt wurde, und die nun Liste nun sicher ist, haben wir
den "Fehler" (es muss nicht der Fehler sein, der rausgenommen wurde) gefunden, und können die Funktion "abbrechen" - das geschieht durch das || in der Rekursion wo die eigentliche Prüfung vorne geschieht
 -}
isSafe2 :: [Int] -> Bool
isSafe2 [x] = True
isSafe2 [x, xs] = isSafeIncDec x xs
isSafe2 xs = isSafe2' [] xs
  where
    isSafe2' _ [] = False
    isSafe2' x (y : ys) = isSafe (x ++ ys) || isSafe2' (x ++ [y]) ys

checkDirection :: Int -> Int -> Direction
checkDirection x y
  | x < y = Up
  | x > y = Down
  | otherwise = Plateau

dayTwoReadFile :: IO [[Int]]
dayTwoReadFile = do
  input <- readFile "src/Day2/input.txt"
  return $ map parseLine (lines input)

parseLine :: String -> [Int]
parseLine line = map read $ words line
