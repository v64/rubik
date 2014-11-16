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
getFr2Br cc = 1

getUrf2Dlf :: CubieCube -> Int
getUrf2Dlf cc = 1

getUr2Ul :: CubieCube -> Int
getUr2Ul cc = 1

getUb2Df :: CubieCube -> Int
getUb2Df cc = 1

getUr2Df :: CubieCube -> Int
getUr2Df cc = 1

choose :: (RealFrac a, Enum a) => a -> a -> Int
choose n k  = (fromIntegral $ truncate p) :: Int
    where p = product [(n + 1 - i) / i | i <- [1..k]]

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
