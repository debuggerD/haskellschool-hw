-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testEx2 :: (Integer, [Integer]) -> Bool
testEx2 (n, ds) = toRevDigits n == ds

ex2Tests :: [Test]
ex2Tests = [ Test "ex2 test" testEx2
             [(1234, [4, 3, 2, 1]), (0, []), ((-17), [])]
           ]

-- Exercise 3 -----------------------------------------

testEx3 :: ([Integer], [Integer]) -> Bool
testEx3 (xs, ys) = doubleEveryOther xs == ys

ex3Tests :: [Test]
ex3Tests = [ Test "ex3 test" testEx3
             [([4, 9, 5, 5], [4, 18, 5, 10]), ([0, 0], [0, 0])]
           ]

-- Exercise 4 -----------------------------------------

testEx4 :: ([Integer], Integer) -> Bool
testEx4 (xs, s) = sumDigits xs == s

ex4Tests :: [Test]
ex4Tests = [ Test "ex4 test" testEx4
             [([10, 5, 18, 4], 19)]
           ]

-- Exercise 5 -----------------------------------------

testEx5 :: (Integer, Bool) -> Bool
testEx5 (x, r) = luhn x == r

ex5Tests :: [Test]
ex5Tests = [ Test "ex5 test" testEx5
             [(5594589764218858, True), (1234567898765432, False)]
           ]

-- Exercise 6 -----------------------------------------

testEx6 :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testEx6 (n, a, b, c, r) = hanoi n a b c == r

ex6Tests :: [Test]
ex6Tests = [ Test "ex6 test" testEx6
             [(2, "a", "b", "c", [("a","c"), ("a","b"), ("c","b")])]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
