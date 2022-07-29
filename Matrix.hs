import Data.List

data Matrix a =
    Matrix
        { rows :: Int
        , cols :: Int
        , nums :: [[a]]
        }
    deriving (Eq, Show)

getEntry :: Matrix a -> Int -> Int -> a
getEntry matrix row col = nthRow matrix row !! col

buildMatrix :: [[a]] -> Matrix a
buildMatrix nums
    | null nums = Matrix 0 0 []
    | otherwise = Matrix rows cols nums
  where
    rows = length nums
    cols = length (head nums)

buildMatrixf :: Int -> (Int -> Int -> a) -> Matrix a
buildMatrixf n f = Matrix n n [[f i j | j <- [1 .. n]] | i <- [1 .. n]]

scaleRow :: Floating a => [a] -> a -> [a]
scaleRow nums factor = map (factor *) nums

nthRow :: Matrix a -> Int -> [a]
nthRow matrix r = nums matrix !! r

nthCol :: Matrix a -> Int -> [a]
nthCol matrix c = [nthRow matrix row !! c | row <- [0 .. rows matrix - 1]]

replaceRow :: Matrix a -> Int -> [a] -> Matrix a
replaceRow matrix row newRow =
    Matrix
        (rows matrix)
        (cols matrix)
        [ if i == row
            then newRow
            else nthRow matrix i
        | i <- [0 .. rows matrix - 1]
        ]

rowAdd :: Num a => [a] -> [a] -> [a]
rowAdd = zipWith (+)

setElement :: [a] -> Int -> a -> [a]
setElement xs i x = prefix ++ x : suffix
  where
    (prefix, _:suffix) = splitAt i xs

setEntry :: Matrix a -> Int -> Int -> a -> Matrix a
setEntry matrix i j x = replaceRow matrix i $ setElement (nthRow matrix i) j x

zeroMatrix :: Num a => Int -> Matrix a
zeroMatrix n = buildMatrixf n (\_ _ -> 0)

identityMatrix :: Num a => Int -> Matrix a
identityMatrix n =
    buildMatrixf
        n
        (\i j ->
             if i == j
                 then 1
                 else 0)

hilbertMatrix :: Floating a => Int -> Matrix a
hilbertMatrix n = buildMatrixf n (\i j -> 1 / fromIntegral (i + j - 1))

isZero :: (Ord a, Floating a) => a -> Bool
isZero x = abs x <= 1e-10

eliminateEntry :: (Ord a, Floating a) => Matrix a -> Int -> Int -> Matrix a
eliminateEntry matrix i j =
    let pivot = getEntry matrix j j
        eliminateFactor = -(getEntry matrix i j / pivot)
        newRow =
            rowAdd
                (nthRow matrix i)
                (scaleRow (nthRow matrix j) eliminateFactor)
     in if isZero pivot
            then error "zero pivot found"
            else replaceRow matrix i newRow

luFactor :: (Ord a, Floating a) => Matrix a -> (Matrix a, Matrix a)
luFactor mat = last $ unfoldr f ((identityMatrix (rows mat), mat), (1, 0))
  where
    f ((l, matrix), (i, j))
        | j == cols matrix = Nothing
        | i == rows matrix = Nothing
        | i + 1 == rows matrix =
            Just ((nextL, nextU), ((nextL, nextU), (j + 2, j + 1)))
        | otherwise = Just ((nextL, nextU), ((nextL, nextU), (succ i, j)))
      where
        nextU = eliminateEntry matrix i j
        pivot = getEntry matrix j j
        eliminateFactor = getEntry matrix i j / pivot
        nextL = setEntry l i j eliminateFactor

example :: IO ()
example = do
    let matrix = buildMatrix [[2, 4, -2], [1, -2, 1], [4, -4, 8]]
    print $ fst $ luFactor matrix
    print $ snd $ luFactor matrix

zeroExample :: IO ()
zeroExample = do
    let matrix = zeroMatrix 3
    print $ luFactor matrix

content :: IO ()
content = do
    let matrix = buildMatrix [[1, 2, -1], [2, 1, -2], [-3, 1, 1]]
    print $ eliminateEntry matrix 1 0
    print $ eliminateEntry (eliminateEntry matrix 1 0) 2 0
    print $ eliminateEntry (eliminateEntry (eliminateEntry matrix 1 0) 2 0) 2 1
    print $ fst $ luFactor matrix
    print $ snd $ luFactor matrix

main :: IO ()
main = do
    let matrix = buildMatrix [[2, 1, 5], [4, 4, -4], [1, 3, 1]]
    print $ fst $ luFactor matrix
    print $ snd $ luFactor matrix
