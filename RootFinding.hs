module RootFinding where

secant :: (Floating a, Ord a) => (a -> a) -> a -> a -> Int -> [a]
secant f x0 x1 n
    | n == 0 = [x2]
    | otherwise = x2 : secant f x1 x2 (pred n)
  where
    slope = (f x1 - f x0) / (x1 - x0)
    x2 = x1 - f x1 / slope

inverseQuadraticInterpolation ::
       (Floating a, Ord a) => (a -> a) -> a -> a -> a -> Int -> [a]
inverseQuadraticInterpolation f x0 x1 x2 n
    | n == 0 = [x3]
    | x1 == x2 = [x2]
    | otherwise = x3 : inverseQuadraticInterpolation f x1 x2 x3 (pred n)
  where
    q = f x0 / f x1
    r = f x2 / f x1
    s = f x2 / f x0
    num = product [r, r - q, x2 - x1] + product [1 - r, s, x2 - x0]
    den = product [q - 1, r - 1, s - 1]
    x3 = x2 - num / den

falsePosition :: (Floating a, Ord a) => (a -> a) -> a -> a -> Int -> [a]
falsePosition f x0 x1 n
    | n == 0 = [x2]
    | f x2 == 0 = [x2]
    | f x0 * f x2 < 0 = x2 : falsePosition f x0 x2 (pred n)
    | otherwise = x2 : falsePosition f x2 x1 (pred n)
  where
    x2 = (x1 * f x0 - x0 * f x1) / (f x0 - f x1)

example = do
    print $ secant (\x -> x ^ 3 + x - 1) 0 1 5
    print $ inverseQuadraticInterpolation (\x -> x ^ 3 + x - 1) 0 1 2 10
    print $ secant (\x -> 2 * x ^ 3 - x - 7) 1 2 2

e1 = do
    print $ secant (\x -> x ^ 3 - 2 * x - 2) 1 2 2
    print $ secant (\x -> exp x + x - 7) 1 2 2
    print $ secant (\x -> exp x + sin x - 4) 1 2 2

e3 = do
    print $ inverseQuadraticInterpolation (\x -> x ^ 3 - 2 * x - 2) 1 2 0 5
    print $ inverseQuadraticInterpolation (\x -> exp x + x - 7) 1 2 0 5
    print $ inverseQuadraticInterpolation (\x -> exp x + sin x - 4) 1 2 0 5

c2 = do
    print $ secant (\x -> x ^ 3 - 2 * x - 2) 1 2 5
    print $ secant (\x -> exp x + x - 7) 1 2 5
    print $ secant (\x -> exp x + sin x - 4) 1 2 5

-- main :: IO ()
-- main = c2
