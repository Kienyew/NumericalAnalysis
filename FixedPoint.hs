module FixedPoint where

import Data.Function (fix)

fixedPoint :: (Ord a, Floating a) => (a -> a) -> a -> a -> a
fixedPoint g x0 tol = iter x0 (g x0) 0
  where
    iter x1 x2 k
        | k >= 10000 = error "too many iterations"
        | abs (x1 - x2) <= tol = (x1 + x2) / 2
        | otherwise = iter x2 (g x2) (k + 1)

e17 :: IO ()
e17 = do
    print $ fixedPoint (\x -> negate . sqrt $ (1 - x) / 2) 1 0.5e-6

e19 :: IO ()
e19 = do
    let a = 14378
    let cbrt_a = fixedPoint (\x -> 0.5 * (x + a * x ** (-2))) 1 0.5e-6
    print $ cbrt_a ^ 3

c1 :: IO ()
c1 = do
    print $ fixedPoint (\x -> (x ^ 3 - 2 * x - 2) / (-14) + x) 1.6 0.5e-8
    print $ fixedPoint (\x -> log (7 - x)) 1.5 0.5e-8
    print $ fixedPoint (\x -> log (4 - sin x)) 1 0.5e-8

c5 :: IO ()
c5 = do
    print $ fixedPoint (\x -> (cos x) ^ 2) 1 0.5e-8

-- main :: IO ()
-- main = c1
