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
cornerParity cc = 1

getFr2Br :: CubieCube -> Int
getFr2Br cc = 24 * a + b
    where a = getFr2BrA cc
          b = getFr2BrB cc

getFr2BrA :: CubieCube -> Int
getFr2BrA cc = getFr2BrA' 0 $ reverse $ ep cc

getFr2BrA' :: Int -> [Edge] -> Int
getFr2BrA' _ []     = 0
getFr2BrA' x (e:es) = e' + getFr2BrA' x' es
    where (e',x')   = if fri <= ei && ei <= bri then (c, (x+1)) else (0, x)
          c         = (11-i) `choose` (x+1)
          i         = length es
          ei        = fromEnum e
          fri       = fromEnum FR
          bri       = fromEnum BR

getFr2BrB :: CubieCube -> Int
getFr2BrB cc = getFr2BrB' (getEdge4 cc) 3 0

getFr2BrB' :: [Edge] -> Int -> Int -> Int
getFr2BrB' _  0 b = b
getFr2BrB' es i b = getFr2BrB' es' i' b'
    where (k,es') = rotateEdge4 es i
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

rotateEdge4 :: [Edge] -> Int -> (Int, [Edge])
rotateEdge4 es i = rotateEdge4' 0 es i

rotateEdge4' :: Int -> [Edge] -> Int -> (Int, [Edge])
rotateEdge4' k es i = if ep /= i+8 then rotateEdge4' k' es' i else (k,es)
    where ep  = fromEnum $ es !! i
          k'  = k+1
          es' = rotateLeft 0 i es

getUrf2Dlf :: CubieCube -> Int
getUrf2Dlf cc = 1

getUr2Ul :: CubieCube -> Int
getUr2Ul cc = 1

getUb2Df :: CubieCube -> Int
getUb2Df cc = 1

getUr2Df :: CubieCube -> Int
getUr2Df cc = 1

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
