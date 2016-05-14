{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches xs ys = 
  foldl (\c (x, y) -> if x == y then c + 1 else c) 0 (zip xs ys)

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors [] = [0, 0, 0, 0, 0, 0]
countColors xs = 
  let incList inc = \ys -> let (ress, _) = foldl (
                                                   \(rs, i) x -> 
                                                     (rs ++ [if inc == i then x + 1 else x], i + 1)
                                                 ) ([], 1) ys
                           in ress
  in
  let (pegIndexs, _) = foldl (
                               \(table, i) x -> 
                                 (\y -> if x == y then i else table y, i + 1)
                             ) (\_ -> 0, 1) colors 
  in
    foldl (\cs x -> incList (pegIndexs x) cs) (countColors []) xs

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum $ map (\(x, y) -> min x y) (zip (countColors xs) (countColors ys))

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secrets guesss = 
  let exactCount = exactMatches secrets guesss in
    Move guesss exactCount ((matches secrets guesss) - exactCount)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guesss ec nc) secrets = 
  let Move _ exactCount nonExactCount = getMove secrets guesss in
    ((ec == exactCount) && (nc == nonExactCount))

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m cs = filter (\x -> isConsistent m x) cs

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n 
  | n <= 0    = [[]]
  | otherwise = concatMap (\xs -> map (\y -> xs ++ [y]) colors) (allCodes (n - 1))

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secrets = 
  let problemSize = length secrets in
  let solve' candidates oldMoves = case candidates of
                                     [] -> oldMoves
                                     x : xs -> let m = getMove secrets x in
                                               let moves = oldMoves ++ [m] in
                                                 case m of
                                                   Move _ exactCount _ | exactCount == problemSize -> moves
                                                   _ -> solve' (filterCodes m xs) moves
  in
    solve' (allCodes problemSize) []

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
