module Newton where

derivative :: Fractional a => (a -> a) -> a -> (a -> a)
derivative f dx x = (f (x + dx / 2) - f (x - dx / 2)) / dx

newton :: (Ord a, Floating a) => (a -> a) -> a -> a -> [a]
newton f x0 tol = iter x0 0
  where
    f' = derivative f 0.00001
    iter x k
        | abs (f x) <= tol = [x]
        | k > 10000 = []
        | otherwise = x : iter (x - f x / f' x) (k + 1)

modifiedNewton :: (Ord a, Floating a) => (a -> a) -> a -> a -> a -> [a]
modifiedNewton f m x0 tol = iter x0 0
  where
    f' = derivative f 0.00001
    iter x k
        | abs (f x) <= tol = [x]
        | k > 10000 = []
        | otherwise = x : iter (x - m * f x / f' x) (k + 1)

example :: IO ()
example = do
    print $ last $ newton (\x -> x ^ 3 + x - 1) (-0.7) 0.5e-6
    print $ last $ newton (^ 2) 1.5 0.5e-30
    print $ newton (\x -> sin x + x ^ 2 * cos x - x ^ 2 - x) 1.0 0.5e-6
    print $
        modifiedNewton (\x -> sin x + x ^ 2 * cos x - x ^ 2 - x) 3 1.0 0.5e-6
    print $ newton (\x -> 4 * x ^ 4 - 6 * x ^ 2 - 11 / 4) 0.3 0.5e-6
    print $ newton (\x -> x ^ 3 - 2 * x ^ 2 + 2 - 1 / x) 0.5 0.5e-10
    print $ errors 1 $ newton (\x -> x ^ 3 - 2 * x ^ 2 + 2 - 1 / x) 59 0.5e-10
    print $ newton (\x -> x ^ 3 - 2 * x ^ 2 + 2 - 1 / x) (-41) 0.5e-10
    print $
        quadraticErrors (-1) $
        newton (\x -> x ^ 3 - 2 * x ^ 2 + 2 - 1 / x) (-41) 0.5e-10

errors :: Floating a => a -> [a] -> [a]
errors r [x1, x2] = [abs (x2 - r) / abs (x1 - r)]
errors r (x1:x2:xs) = e2 / e1 : errors r (x2 : xs)
  where
    e2 = abs (x2 - r)
    e1 = abs (x1 - r)

quadraticErrors :: Floating a => a -> [a] -> [a]
quadraticErrors r [x1, x2] = [abs (x2 - r) / (x1 - r) ^ 2]
quadraticErrors r (x1:x2:xs) = e2 / e1 ^ 2 : quadraticErrors r (x2 : xs)
  where
    e2 = abs (x2 - r)
    e1 = abs (x1 - r)

e1 = do
    print $ newton (\x -> x ^ 3 + x - 2) 0 0.5e-6
    print $ newton (\x -> x ^ 4 - x ^ 2 + x - 1) 0 0.5e-6
    print $ newton (\x -> x ^ 2 - x - 1) 0 0.5e-6

c1 = do
    print $ newton (\x -> x ^ 3 - 2 * x - 2) 1 0.5e-8
    print $ newton (\x -> exp x + x - 7) 1 0.5e-8
    print $ newton (\x -> exp x + sin x - 4) 1 0.5e-8

-- silo, height = 10, hemispherical dome volume = 400
-- 2/3 pi r^3 + 10 pi r^2 = 400
c5 = do
    print $ newton (\r -> 2 / 3 * pi * r ^ 3 + 10 * pi * r ^ 2 - 400) 1 0.5e-8

c7 = do
    print $ newton f (-2) 0.5e-8
    print $ newton f 2 0.5e-8
    print $ newton f 1 0.5e-8
  where
    f x = (exp . (^ 3) . sin) x + x ^ 6 - 2 * x ^ 4 - x ^ 3 - 1

c9 = do
    let f x =
            14 * x * exp (x - 2) - 12 * exp (x - 2) - 7 * x ^ 3 + 20 * x ^ 2 -
            26 * x +
            12
    print $ newton f 0.5 0.5e-8
    print $ quadraticErrors 0.8571428571428568 $ newton f 0.5 0.5e-8
    print $ newton f 1.5 0.5e-20
    print $ errors 2 $ newton f 1.5 0.5e-20

c11 = do
    let r = 0.0820578
        a = 1.36
        b = 0.003183
        t = 320
        p = 15
        n = 1
        vanderwaals v = (p + n ^ 2 * a / v ^ 2) * (v - n * b) - n * r * t
        v0 = (n * r * t) / p
    print $ newton vanderwaals v0 0.5e-8

c13 = do
    let f x = (1 - 3 / (4 * x)) ** (1 / 3)
    print $ take 10 $ newton f 0.74 1e-9

-- main :: IO ()
-- main = c13
