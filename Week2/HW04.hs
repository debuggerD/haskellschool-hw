{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [fromInteger 0, fromInteger 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    P (l:ls) == P (r:rs) = if l == r then (P ls) == (P rs) else False
    P [] == P [] = True
    _ == _ = False
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P []) = "0"
    show (P cs) = 
      if all (\c -> c == (fromInteger 0)) cs then "0"
      else
        concat . 
        reverse . 
        (intersperse " + ") . 
        (filter (\s -> s /= "")) . 
        (map (\(p, c) -> (
                           if c == (fromInteger 0) then "" 
                           else (
                             if c == (fromInteger 1) && p > 0 then "" 
                             else (
                                    if c == (fromInteger (-1)) && p > 0 then 
                                    "-" else show c
                                  )
                           ) ++ (
                                  if p > 1 then "x^" ++ show p 
                                  else (if p == 1 then "x" else "")
                                )
                         )
             )
        ) . 
        (zip [0 .. ((length cs) - 1)]) 
        $ cs

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P (l:ls)) (P (r:rs)) = P ([l + r] ++ (case plus (P ls) (P rs) of P s -> s))
plus (P []) r = r
plus l (P []) = l

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P []) (P _) = P [fromInteger 0]
times (P _) (P []) = P [fromInteger 0]
times (P ls) (P rs) = 
  let funPolyLeft = foldr (\c f -> (\p -> if p == 0 then c else f (p - 1))) (\_ -> fromInteger 0) ls in
  let funPolyMult = foldr (
                            \(multPow, c) f -> 
                              (
                                \p -> (c * (funPolyLeft (p - multPow))) + (f p)
                              )
                          ) (\_ -> fromInteger 0) (zip [0 .. ((length rs) - 1)] rs) in
  let maxPower = ((length ls) - 1) * ((length rs) - 1) + 1 in
    P (unfoldr (\p -> if p > maxPower then Nothing else Just (funPolyMult p, p + 1)) 0)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P cs) = P (map negate cs)
    fromInteger a = P [fromInteger a]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P []) _ = 0
applyP (P (c:cs)) v = c + v * (applyP (P cs) v)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n f = 
      case n of
        _ | n < 1 -> f
        _ -> nderiv (n - 1) (deriv f)

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P []) = fromInteger 0
    deriv (P [_]) = fromInteger 0
    deriv (P cs) = P (
                   tail . 
                   (map (\(p, c) -> c * (fromInteger p))) . 
                   (zip [0 .. ]) 
                   $ cs
                 )

