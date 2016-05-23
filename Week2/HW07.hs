{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
--import Data.Functor
import Data.Monoid
import Data.Vector (Vector, (!), (!?), (//))
--import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f = \x -> 
  do
    rx <- x
    return (f rx)

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV position1 position2 targetVec = 
  let r1 = targetVec !? position1
      r2 = targetVec !? position2
      buildSubstituteList = liftM2 (\a b -> [(position1, b), (position2, a)])
      applySubstitute = liftM2 (\v s -> v // s)
  in
    applySubstitute (Just targetVec) (buildSubstituteList r1 r2)

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f [x] = f x >>= (\z -> return [z])
mapM f (x : xs) = mapM f xs >>= (\ys -> f x >>= \z -> return (z : ys))

getElts :: [Int] -> Vector a -> Maybe [a]
getElts xs v = mapM (\i -> v !? i) xs

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = 
  if (V.length v) == 0 then return Nothing
  else 
    do
      i <- getRandomR (0, (V.length v) - 1)
      return (v !? i)

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = V.generateM n (\_ -> getRandom)

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n range = V.generateM n (\_ -> getRandomR range)

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = 
  let n = V.length v
      iv = V.fromList . reverse $ [1 .. (n - 1)]
      jv = V.generateM n (\idx -> getRandomR (0, iv ! idx))
      _ = jv `asTypeOf` (randomVec 0)
      shuffles = do rjv <- jv; V.zipWithM (\x y -> return (x, y)) iv rjv
  in
    do
      rshuffles <- shuffles
      V.foldM (
                \curV (x, y) -> return (
                                      case swapV x y curV of
                                        Just resV -> resV
                                        _ -> curV
                                    )
              ) v rshuffles

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v pivotIdx = 
  let pivot = v ! pivotIdx
      (lesss, _, greaterequals) = V.foldl (
                                            \(ls, isPivoted, ges) x -> 
                                              if (not isPivoted) && (x == pivot) then (ls, True, ges) 
                                              else
                                                (
                                                  if x < pivot then (ls ++ [x], isPivoted, ges) 
                                                  else (ls, isPivoted, ges ++ [x])
                                                )
                                          ) ([], False, []) v
  in
    (V.fromList lesss, pivot, V.fromList greaterequals)


-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v = 
  if (V.length v) <= 1 then v
  else
    let vTail = V.tail v in
      V.concat [(qsort [ y | y <- vTail, y < v ! 0 ])
                , (V.take 1 v)
                , (qsort [ y | y <- vTail, y >= v ! 0 ])]

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v = 
  let n = V.length v in
    if n <= 1 then return v
    else
      do
        (subLesss, pivot, subGreaterEquals) <- (liftM (partitionAt v)) (getRandomR (0, n - 1))
        sortedSubLesss <- qsortR subLesss
        sortedSubGreaterEquals <- qsortR subGreaterEquals
        return (V.concat [sortedSubLesss, (V.singleton pivot), sortedSubGreaterEquals])

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select rank v = 
  let n = V.length v in
    if rank < 0 || rank >= n then return Nothing
    else
      if rank == 0 && n == 1 then return (v !? rank)
      else
        do
          (subLesss, pivot, subGreaterEquals) <- (liftM (partitionAt v)) (getRandomR (0, n - 1))
          let sizeL = V.length subLesss in
            if rank == sizeL then return (Just pivot)
            else
              if rank < sizeL then select rank subLesss
              else select (rank - sizeL - 1) subGreaterEquals

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [ Card x y | x <- labels, y <- suits ]

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard v = 
  if V.length v == 0 then Nothing
  else Just (V.head v, V.tail v)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n v = foldl (
                       \hand _ -> 
                         hand >>= (
                                    \(currentHand, currentDeck) -> 
                                      (nextCard currentDeck) >>= (
                                                                   \(drawCard, drawDeck) -> 
                                                                     Just (currentHand ++ [drawCard], drawDeck)
                                                                 )
                                  )
                     ) (Just ([], v)) [1 .. n] 

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
