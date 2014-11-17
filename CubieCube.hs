module CubieCube (
    CubieCube (..),
    toCoordCube
) where

import CoordCube
import Corner
import Edge

data CubieCube = CubieCube {
    cp :: [Corner],
    co :: [Int],
    ep :: [Edge],
    eo :: [Int]
} deriving (Eq, Show)

toCoordCube :: CubieCube -> CoordCube
toCoordCube cc = CoordCube {
    ctwist  = getCtwist    cc,
    eflip   = getEflip     cc,
    parity  = cornerParity cc,
    fr2br   = getFr2Br     cc,
    urf2dlf = getUrf2Dlf   cc,
    ur2ul   = getUr2Ul     cc,
    ub2df   = getUb2Df     cc,
    ur2df   = getUr2Df     cc
}

getCtwist :: CubieCube -> Int
getCtwist cc = foldl (\n x -> 3 * n + x) 0 $ init $ co cc

getEflip :: CubieCube -> Int
getEflip cc = foldl (\n x -> 2 * n + x) 0 $ init $ eo cc

cornerParity :: CubieCube -> Int
cornerParity cc = -1

getFr2Br :: CubieCube -> Int
getFr2Br cc = 24 * a + b
    where a = getFr2BrA cc
          b = getFr2BrB cc

getFr2BrA :: CubieCube -> Int
getFr2BrA cc = getFr2BrA' 0 $ reverse $ ep cc

getFr2BrA' :: Int -> [Edge] -> Int
getFr2BrA' _ []     = 0
getFr2BrA' x (e:es) = e' + getFr2BrA' x' es
    where (e',x')   = if fri <= ei && ei <= bri then (ch, (x+1)) else (0, x)
          ch        = (11-i) `choose` (x+1)
          i         = length es
          ei        = fromEnum e
          fri       = fromEnum FR
          bri       = fromEnum BR

getFr2BrB :: CubieCube -> Int
getFr2BrB cc = getFr2BrB' (getEdge4 cc) 3 0

getFr2BrB' :: [Edge] -> Int -> Int -> Int
getFr2BrB' _  0 b = b
getFr2BrB' es i b = getFr2BrB' es' i' b'
    where (es',k) = rotateEdge4 es i
          i'      = (i-1)
          b'      = (i+1) * b + k

getEdge4 :: CubieCube -> [Edge]
getEdge4 cc = reverse $ getEdge4' $ reverse $ ep cc

getEdge4' :: [Edge] -> [Edge]
getEdge4' []     = []
getEdge4' (e:es) = e' ++ getEdge4' es
    where e'     = if fri <= ei && ei <= bri then [e] else []
          ei     = fromEnum e
          fri    = fromEnum FR
          bri    = fromEnum BR

rotateEdge4 :: [Edge] -> Int -> ([Edge], Int)
rotateEdge4 es i = rotateEdge4' 0 es i

rotateEdge4' :: Int -> [Edge] -> Int -> ([Edge], Int)
rotateEdge4' k es i = if ep /= i+8 then rotateEdge4' k' es' i else (es,k)
    where ep  = fromEnum $ es !! i
          k'  = k+1
          es' = rotateLeft 0 i es

getUrf2Dlf :: CubieCube -> Int
getUrf2Dlf cc = 720 * a + b
    where a = getUrf2DlfA cc
          b = getUrf2DlfB cc

getUrf2DlfA :: CubieCube -> Int
getUrf2DlfA cc = getUrf2DlfA' 0 $ cp cc

getUrf2DlfA' :: Int -> [Corner] -> Int
getUrf2DlfA' _ [] = 0
getUrf2DlfA' x (c:cs) = c' + getUrf2DlfA' x' cs
    where (c',x') = if ci <= dlfi then (ch, (x+1)) else (0,x)
          ch      = i `choose` (x+1)
          i       = 7 - length cs
          ci      = fromEnum c
          dlfi    = fromEnum DLF

getUrf2DlfB :: CubieCube -> Int
getUrf2DlfB cc = getUrf2DlfB' (getCorner6 cc) 5 0

getUrf2DlfB' :: [Corner] -> Int -> Int -> Int
getUrf2DlfB' _  0 b = b
getUrf2DlfB' cs i b = getUrf2DlfB' cs' i' b'
    where (cs',k)   = rotateCorner6 cs i
          i'        = (i-1)
          b'        = (i+1) * b + k

getCorner6 :: CubieCube -> [Corner]
getCorner6 cc = getCorner6' $ cp cc

getCorner6' :: [Corner] -> [Corner]
getCorner6' []     = []
getCorner6' (c:cs) = c' ++ getCorner6' cs
    where c'       = if ci <= dlfi then [c] else []
          ci       = fromEnum c
          dlfi     = fromEnum DLF

rotateCorner6 :: [Corner] -> Int -> ([Corner], Int)
rotateCorner6 cs i = rotateCorner6' 0 cs i

rotateCorner6' :: Int -> [Corner] -> Int -> ([Corner], Int)
rotateCorner6' k cs i = if cp /= i then rotateCorner6' k' cs' i else (cs,k)
    where cp  = fromEnum $ cs !! i
          k'  = k+1
          cs' = rotateLeft 0 i cs

getUr2Ul :: CubieCube -> Int
getUr2Ul cc = 6 * a + b
    where a = getUr2UlA cc
          b = getUr2UlB cc

getUr2UlA :: CubieCube -> Int
getUr2UlA cc = getUr2UlA' 0 $ ep cc

getUr2UlA' :: Int -> [Edge] -> Int
getUr2UlA' _ []     = 0
getUr2UlA' x (e:es) = e' + getUr2UlA' x' es
    where (e',x') = if ei <= uli then (ch, (x+1)) else (0, x)
          ch      = i `choose` (x+1)
          i       = 11 - length es
          ei      = fromEnum e
          uli     = fromEnum UL

getUr2UlB :: CubieCube -> Int
getUr2UlB cc = getUr2UlB' (getEdge3 cc) 2 0

getUr2UlB' :: [Edge] -> Int -> Int -> Int
getUr2UlB' _  0 b = b
getUr2UlB' es i b = getUr2UlB' es' i' b'
    where (es',k) = rotateEdge3 es i
          i'      = (i-1)
          b'      = (i+1) * b + k

getEdge3 :: CubieCube -> [Edge]
getEdge3 cc = getEdge3' $ ep cc

getEdge3' :: [Edge] -> [Edge]
getEdge3' []     = []
getEdge3' (e:es) = e' ++ getEdge3' es
    where e'  = if ei <= uli then [e] else []
          ei  = fromEnum e
          uli = fromEnum UL

rotateEdge3 :: [Edge] -> Int -> ([Edge], Int)
rotateEdge3 es i = rotateEdge3' 0 es i

rotateEdge3' :: Int -> [Edge] -> Int -> ([Edge], Int)
rotateEdge3' k es i = if ep /= i then rotateEdge3' k' es' i else (es,k)
    where ep  = fromEnum $ es !! i
          k'  = k+1
          es' = rotateLeft 0 i es

getUb2Df :: CubieCube -> Int
getUb2Df cc = -1

getUr2Df :: CubieCube -> Int
getUr2Df cc = -1

choose :: Int -> Int -> Int
choose n k
    | n == k    = 1
    | n == 0    = 0
    | k == 0    = 1
    | otherwise = (chooseIndex !! (n-1) !! (k-1)) +
                  (chooseIndex !! (n-1) !! k)

chooseIndex :: [[Int]]
chooseIndex = [[choose n k | k <- [0..]] | n <- [0..]]

slice :: Int -> Int -> [a] -> [a]
slice a b xs = take (b-a+1) . drop a $ xs

takeR :: Int -> [a] -> [a]
takeR n l = go (drop n l) l
    where go []     r      = r
          go (_:xs) (_:ys) = go xs ys

rotate :: ([a] -> [a]) -> Int -> Int -> [a] -> [a]
rotate f a b xs = (take a xs) ++ (f $ slice a b xs) ++ (takeR (length xs - b - 1) xs)

rotateLeft :: Int -> Int -> [a] -> [a]
rotateLeft = rotate (\xs -> tail xs ++ [head xs])

rotateRight :: Int -> Int -> [a] -> [a]
rotateRight = rotate (\xs -> last xs : init xs)
