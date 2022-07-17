module Main where

import Lucid.Bootstrap (span9_)

evaluatePolynomial :: Num a => [a] -> a -> a
evaluatePolynomial coeffs x0 = sum [coeffs !! k * x0 ^ k | k <- [0 .. n]]
  where
    n = length coeffs - 1

bisection :: (Ord a, Fractional a) => (a -> a) -> a -> a -> a -> a
bisection f a b tol
    | fa * fb >= 0 = error "f(a) and f(b) have same signs"
    | (b - a) / 2 <= tol = c
    | otherwise =
        if fa * fc < 0
            then bisection f a c tol
            else bisection f c b tol
  where
    c = (a + b) / 2
    fa = f a
    fb = f b
    fc = f c

q3 :: IO ()
q3
    -- 3(a) x^3 == 9
    -- 3(b) 3x^3 + x^2 = x
    -- 3(c) cos(x)^2 + 6 = x
 = do
    print $ bisection (\x -> x ^ 3 - 9) 2 3 0.125
    print $ bisection (\x -> 3 * x ^ 3 + x ^ 2 - x - 5) 1 2 0.125
    print $ bisection (\x -> cos x ^ 2 + 6 - x) 6 7 0.125

q5 :: IO ()
q5 = do
    print $ bisection (\x -> x ^ 4 - x ^ 3 - 10) a b 1e-10 -- require 33 iterations
  where
    a = 2
    b = 3

c5 :: IO ()
c5 = do
    print $ bisection (\x -> x ^ 3 - 2) 1 2 0.5e-8
    print $ bisection (\x -> x ^ 3 - 3) 1 2 0.5e-8
    print $ bisection (\x -> x ^ 3 - 5) 1 2 0.5e-8
    -- all starting interval is [1, 2]
    -- all requires 27 step

c7 :: IO ()
c7 = do
    print (x1, f x1 + 1000)
    print (x2, f x2 + 1000)
  where
    f x = x ^ 4 - 202 * x ^ 2 + 1404 * x - 2475 - 1000
    x1 = bisection f (-18) (-17) 0.5e-6
    x2 = bisection f 9 10 0.5e-6

c9 :: IO ()
c9 = do
    print $ bisection (\h -> volume h - 1) 0 1 0.001
  where
    r = 1
    volume h = pi * h ^ 2 * (r - 1 / 3 * h)

main :: IO ()
main
    -- print $ bisection (evaluatePolynomial [-1, 1, 0, 1]) 0 1 0.00005
    -- print $ bisection (\x -> cos x - x) 0 1 0.5e-6
    -- 2x^3 - x - 7
    -- print $ bisection (evaluatePolynomial [-7, -1, 0, 2]) 1 2 0.00005
    -- exp(x) == 3
    -- print $ bisection (\x -> exp x - 3) 0 2 0.5e-6
    -- q3
    -- q5
    -- c5
    -- c7
 = do
    c9
