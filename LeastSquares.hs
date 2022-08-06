module LeastSquares where

import Data.Matrix

solveNormal :: (Fractional a, Eq a) => Matrix a -> Matrix a -> Matrix a
solveNormal a b = lhs `multStd` rhs
  where
    lhs =
        case inverse $ multStd (transpose a) a of
            Left e -> error e
            Right inv -> inv
    rhs = multStd (transpose a) b

se :: Floating a => (a -> a) -> [a] -> [a] -> a
se model xs ys = sum $ zipWith (\y1 y2 -> (y1 - y2) ^ 2) (map model xs) ys

twoNorm :: Floating a => (a -> a) -> [a] -> [a] -> a
twoNorm model xs ys = sqrt (se model xs ys)

rmse :: Floating a => (a -> a) -> [a] -> [a] -> a
rmse model xs ys = sqrt (se model xs ys) / sqrt (fromIntegral (length xs))

e11 :: IO ()
e11 = do
    let a = fromLists [[1, 1], [1, 2], [1, 3], [1, 4]]
        b = fromLists [[139.905], [284.62], [429.145], [563.48]]
    print $ solveNormal a b

c5 :: IO ()
c5 = do
    let a =
            fromLists
                [ [1, 0.59]
                , [1, 0.8]
                , [1, 0.95]
                , [1, 0.45]
                , [1, 0.79]
                , [1, 0.99]
                , [1, 0.90]
                , [1, 0.65]
                , [1, 0.79]
                , [1, 0.69]
                , [1, 0.79]
                , [1, 0.49]
                , [1, 1.09]
                , [1, 0.95]
                , [1, 0.79]
                , [1, 0.65]
                , [1, 0.45]
                , [1, 0.60]
                , [1, 0.89]
                , [1, 0.79]
                , [1, 0.99]
                , [1, 0.85]
                ]
        b =
            fromList
                22
                1
                [ 3980
                , 2200
                , 1850
                , 6100
                , 2100
                , 1700
                , 2000
                , 4200
                , 2440
                , 3300
                , 2300
                , 6000
                , 1190
                , 1960
                , 2760
                , 4330
                , 6960
                , 4160
                , 1990
                , 2860
                , 1920
                , 2160
                ]
    print $ solveNormal a b

example :: IO ()
example = do
    let a = fromLists [[4, 2], [2, 6]]
        b = fromLists [[-1], [-5]]
    print $ solveNormal a b
-- main :: IO ()
-- main = c5
