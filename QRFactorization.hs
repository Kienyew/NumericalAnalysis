module QRFactorization where

import qualified Data.Matrix as M
import qualified Data.Vector as V

qrFactor :: (Floating a, Eq a) => M.Matrix a -> (M.Matrix a, M.Matrix a)
qrFactor a =
    ( M.matrix (M.nrows a) (M.ncols a) buildQ
    , M.matrix (M.ncols a) (M.ncols a) buildR)
  where
    buildQ (i, j) = q j V.! (i - 1)
    buildR = r
    norm v = sqrt $ V.foldl (\acc x -> acc + x ^ 2) 0 v
    q j = V.map (/ norm (y j)) (y j)
    y j
        | j == 1 = col
        | otherwise = V.foldl f col qqts
      where
        f v qqt =
            V.zipWith (-) v $
            M.getMatrixAsVector $ M.multStd qqt (M.colVector col)
        col = M.getCol j a
        qqts =
            V.fromList
                [ M.colVector (q i) `M.multStd` M.rowVector (q i)
                | i <- [1 .. j - 1]
                ]
    r (i, j) =
        (M.rowVector (q i) `M.multStd` M.colVector (M.getCol j a)) M.! (1, 1)

fullQrFactor :: (Floating a, Eq a) => M.Matrix a -> (M.Matrix a, M.Matrix a)
fullQrFactor a = (q, M.matrix m n (r M.!))
  where
    m = M.nrows a
    n = M.ncols a
    f (i, j)
        | j <= n = a M.! (i, j)
        | otherwise = fromIntegral $ i * j
    extA = M.matrix m m f
    (q, r) = qrFactor extA

main :: IO ()
main = do
    let a = M.fromLists [[1, -4], [2, 3], [2, 2]]
    print $ (snd . qrFactor) a
    print $ (snd . fullQrFactor) a
