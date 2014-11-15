module CubieCube (
    CubieCube
) where

import Corner
import Edge

data CubieCube = CubieCube {
    cp :: [Corner],
    co :: [Int],
    ep :: [Edge],
    eo :: [Int]
} deriving (Eq, Show)
