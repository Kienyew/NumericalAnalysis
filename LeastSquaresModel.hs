module LeastSquaresModel where

import Data.Matrix
import LeastSquares (rmse, se, solveNormal, twoNorm)

example :: IO ()
example = do
    let a = fromLists [[1, -1], [1, 0], [1, 2], [1, 3]]
        b = fromList 4 1 $ map log [4, 2, 1, 0.5]
        x = solveNormal a b
        model t = a * exp (b * t)
          where
            a = exp (x ! (1, 1))
            b = x ! (2, 1)
    print x
    print $ twoNorm model [-1, 0, 2, 3] [4, 2, 1, 0.5]

e1 :: IO ()
e1 = do
    let xs = [0, 1 / 4, 1 / 2, 3 / 4]
        ys = [1, 3, 2, 1]
        a = fromLists $ map (\t -> [1, cos (2 * pi * t), sin (2 * pi * t)]) xs
        x = solveNormal a $ fromList 4 1 ys
        model t = 1.75 - 0.5 * cos (2 * pi * t) + sin (2 * pi * t)
    print x
    print $ twoNorm model xs ys
    print $ rmse model xs ys

-- y = a t exp(b t)
e5 :: IO ()
e5 = do
    let xs = [1, 1, 2, 3, 5]
        ys = [2, 4, 5, 6, 10]
        b = fromList 5 1 $ zipWith (\y t -> log y - log t) ys xs
        a = fromLists $ map (\t -> [1, t]) xs
        x = solveNormal a b
        model t = a * t * exp (b * t)
          where
            a = exp (x ! (1, 1))
            b = x ! (2, 0)
    print x
    print $ rmse model xs ys

main :: IO ()
main = e5
