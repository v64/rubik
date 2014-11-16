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
    twist   = getTwist     cc,
    orient  = getOrient    cc,
    parity  = cornerParity cc,
    fr2br   = getFr2Br     cc,
    urf2dlf = getUrf2Dlf   cc,
    ur2ul   = getUr2Ul     cc,
    ub2df   = getUb2Df     cc,
    ur2df   = getUr2Df     cc
}

getTwist :: CubieCube -> Int
getTwist cc = getTwist' 0 [0..6] (co cc)

getTwist' :: Int -> [Int] -> [Int] -> Int
getTwist' n []     _  = n
getTwist' n (x:xs) co = getTwist' n' xs co
    where n' = 3 * n + (co !! x)

getOrient :: CubieCube -> Int
getOrient cc = 1

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
