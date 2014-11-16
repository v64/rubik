module CubieCube (
    CubieCube (..),
--    toCoordCube
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

-- toCoordCube :: CubieCube -> CoordCube
