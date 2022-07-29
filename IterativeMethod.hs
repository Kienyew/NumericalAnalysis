module IterativeMethod where

import Data.Matrix

getD :: Num a => Matrix a -> Matrix a
getD mat = diagonal 0 (getDiag mat)
  where
    diag = getDiag mat

getL :: Num a => Matrix a -> Matrix a
getL mat = matrix n n f
  where
    n = nrows mat
    f (i, j) =
        if i <= j
            then 0
            else mat ! (i, j)

getU :: Num a => Matrix a -> Matrix a
getU mat = matrix n n f
  where
    n = nrows mat
    f (i, j) =
        if i >= j
            then 0
            else mat ! (i, j)

matrixAdd :: Num a => Matrix a -> Matrix a -> Matrix a
matrixAdd = elementwise (+)

matrixSub :: Num a => Matrix a -> Matrix a -> Matrix a
matrixSub = elementwise (-)

matrixInv :: (Eq a, Floating a) => Matrix a -> Matrix a
matrixInv mat =
    case inverse mat of
        Left msg -> error msg
        Right m -> m

jacobi ::
       (Eq a, Floating a)
    => Matrix a
    -> Matrix a
    -> Int
    -> Matrix a
    -> [Matrix a]
jacobi a b n x0 = jacobi' n x0
  where
    invD = matrixInv (getD a)
    l = getL a
    u = getU a
    jacobi' n x
        | n == 0 = [x]
        | otherwise = x : jacobi' (pred n) x'
      where
        x' = multStd invD $ matrixSub b $ multStd (matrixAdd l u) x

sor :: (Eq a, Floating a)
    => Matrix a
    -> Matrix a
    -> a
    -> Int
    -> Matrix a
    -> [Matrix a]
sor a b w n x0
    | n == 0 = [x0]
    | otherwise = x0 : sor a b w (pred n) x
  where
    d = getD a
    l = getL a
    u = getU a
    p = matrixInv $ scaleMatrix w l `matrixAdd` d
    q =
        (scaleMatrix (1 - w) d `multStd` x0) `matrixSub`
        (scaleMatrix w u `multStd` x0)
    r = scaleMatrix w (matrixInv $ d `matrixAdd` scaleMatrix w l) `multStd` b
    x = p `multStd` q `matrixAdd` r

sparse :: Floating a => Int -> (Matrix a, Matrix a)
sparse n
    | odd n = error "n is not even"
    | otherwise = (matrix n n f, b)
  where
    f (i, j)
        | i == j = 3
        | abs (i - j) == 1 = -1
        | j == n + 1 - i = 1 / 2
        | otherwise = 0
    g (i, 1)
        | i == 1 || i == n = 2.5
        | i == n `div` 2 || i == n `div` 2 + 1 = 1.0
        | otherwise = 1.5
    b = matrix n 1 g
{--
main :: IO ()
main = do
    let a = fromLists [[3, 1], [1, 2]]
    let b = fromList 2 1 [5, 5]
    print $ last $ jacobi a b 50 $ fromList 2 1 [0, 0]
    print $ last $ sor a b 1 10 $ fromList 2 1 [0, 0]
    let a = fromLists [[3, 1, -1], [2, 4, 1], [-1, 2, 5]]
    let b = fromList 3 1 [4, 1, 1]
    print $ sor a b 1 10 $ fromList 3 1 [0, 0, 0]
    let n = 50
    let (a, b) = sparse n
    print $ last $ jacobi a b 5 $ fromList n 1 [0,0 ..]
    --}
