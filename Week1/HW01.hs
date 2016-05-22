{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits x
  | x <= 0    = []
  | otherwise = lastDigit x : (toRevDigits $ dropLastDigit x)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = 
  let stateTuple = foldl (
                           \(isDouble, rs) x -> 
                             (
                               not isDouble, 
                               rs ++ if isDouble then [x * 2] else [x]
                             )
                         ) (False, []) xs
  in
    case stateTuple of (_, rs) -> rs


-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (\s x -> s + lastDigit x + dropLastDigit x) 0 xs


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = ((lastDigit . sumDigits . doubleEveryOther . toRevDigits $ x) == 0)

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n srcPeg targetPeg tempPeg = 
  hanoi (n - 1) srcPeg tempPeg targetPeg ++ 
  [(srcPeg, targetPeg)] ++ 
  hanoi (n - 1) tempPeg targetPeg srcPeg
