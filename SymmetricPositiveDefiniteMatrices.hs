module SymmetricPositiveDefiniteMatrix where

import Data.Matrix
import IterativeMethod

choleskyFactor :: Floating a => Matrix a -> Matrix a
choleskyFactor m = matrix n n f
  where
    n = nrows m
    a = m ! (1, 1)
    b = matrix (n - 1) 1 $ \(i, j) -> m ! (i + 1, j)
    u = scaleMatrix (1 / sqrt a) b
    c = matrix (n - 1) (n - 1) $ \(i, j) -> m ! (i + 1, j + 1)
    a1 = matrixSub c $ multStd u (transpose u)
    v = matrix (n - 1) (n - 1) (choleskyFactor a1 !)
    f (i, j)
        | (i, j) == (1, 1) = sqrt $ m ! (i, j)
        | j == 1 = 0
        | i == 1 = u ! (j - 1, 1)
        | otherwise = v ! (i - 1, j - 1)

conjugateGradient ::
       Floating a => Matrix a -> Matrix a -> Matrix a -> [Matrix a]
conjugateGradient a b x0 = loop x0 r0 d0 n
  where
    r0 = matrixSub b (multStd a x0)
    d0 = matrixSub b (multStd a x0)
    n = nrows a
    get m = m ! (1, 1)
    loop x r d n
        | n == 0 = [x]
        | otherwise = x : loop x1 r1 d1 (pred n)
      where
        alpha =
            get (transpose r `multStd` r) /
            get (transpose d `multStd` a `multStd` d)
        x1 = x + scaleMatrix alpha d
        r1 = matrixSub r (scaleMatrix alpha $ a `multStd` d)
        beta = get (transpose r1 `multStd` r1) / get (transpose r `multStd` r)
        d1 = r1 + scaleMatrix beta d

main :: IO ()
main = do
    print $ choleskyFactor $ fromLists [[2, 2], [2, 5]]
    print $ choleskyFactor $ fromLists [[4, -2, 2], [-2, 2, -4], [2, -4, 11]]
    print $
        last $
        conjugateGradient
            (fromLists [[2, 2], [2, 5]])
            (fromLists [[6], [3]])
            (fromLists [[0], [0]])
    print $ choleskyFactor $ fromLists [[4, -2], [-2, 6]]
    let r = choleskyFactor $ fromLists [[4, -2], [-2, 6]]
    print $ transpose r `multStd` r
