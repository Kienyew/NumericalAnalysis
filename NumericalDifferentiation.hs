module NumericalDifferentiation where

import Numeric

formatf :: (RealFloat a) => a -> String
formatf f = showFFloat Nothing f ""

twoPointForwardDifference :: (Floating a, Ord a) => (a -> a) -> a -> a -> a
twoPointForwardDifference f h x = (f (x + h) - f x) / h

threePointDifference :: (Floating a, Ord a) => (a -> a) -> a -> a -> a
threePointDifference f h x = (f (x + h) - f (x - h)) / (2 * h)

threePointForwardDifference :: (Floating a, Ord a) => (a -> a) -> a -> a -> a
threePointForwardDifference f h x = (f (x - h) - 2 * f x + f (x + h)) / (h ^ 2)

minmax :: Ord a => (a, a) -> (a, a)
minmax (x, y) =
    if x < y
        then (x, y)
        else (y, x)

richardsonExtrapolation :: (Floating a, Ord a) => (a -> a) -> Int -> (a -> a)
richardsonExtrapolation f n = g
  where
    g h = (2 ^ n * f (h / 2) - f h) / (2 ^ n - 1)

example :: IO ()
example = do
    print $ threePointDifference (exp . cos) 0.1 (pi / 2)
    print $ threePointDifference (exp . cos) 0.01 (pi / 2)

e1 :: IO ()
e1 = do
    print $ twoPointForwardDifference log 0.1 1
    print $ twoPointForwardDifference log 0.01 1
    print $ twoPointForwardDifference log 0.001 1

e3 :: IO ()
e3 = do
    print $ twoPointForwardDifference sin 0.1 (pi / 3)
    print $ twoPointForwardDifference sin 0.01 (pi / 3)
    print $ twoPointForwardDifference sin 0.001 (pi / 3)
    let err h c = minmax (h / 2 * sin c, h / 2 * sin (c + h))
        (emin, emax) = err 0.1 (pi / 3)
    print (formatf emin, formatf emax)
    let (emin, emax) = err 0.01 (pi / 3)
    print (formatf emin, formatf emax)
    let (emin, emax) = err 0.001 (pi / 3)
    print (formatf emin, formatf emax)

main :: IO ()
main = e3
