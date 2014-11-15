module FaceCube (
    strToFaceCube,
    toCubieCube
) where

import Color
import CubieCube
import Facelet

cornerColor :: [[Color]]
cornerColor = [[U, R, F],
               [U, F, L],
               [U, L, B],
               [U, B, R],
               [D, F, R],
               [D, L, F],
               [D, B, L],
               [D, R, B]]

cornerFacelet :: [[Facelet]]
cornerFacelet = [[U9, R1, F3],
                 [U7, F1, L3],
                 [U1, L1, B3],
                 [U3, B1, R3],
                 [D3, F9, R7],
                 [D1, L9, F7],
                 [D7, B9, L7],
                 [D9, R9, B7]]

edgeColor :: [[Color]]
edgeColor = [[U, R],
             [U, F],
             [U, L],
             [U, B],
             [D, R],
             [D, F],
             [D, L],
             [D, B],
             [F, R],
             [F, L],
             [B, L],
             [B, R]]

edgeFacelet :: [[Facelet]]
edgeFacelet = [[U6, R2],
               [U8, F2],
               [U4, L2],
               [U2, B2],
               [D6, R8],
               [D2, F8],
               [D4, L8],
               [D8, B8],
               [F6, R4],
               [F4, L6],
               [B6, L4],
               [B4, R6]]

data FaceCube = FaceCube {
    f :: [Color]
} deriving (Eq, Show)

strToFaceCube :: String -> FaceCube
strToFaceCube s = FaceCube {
    f = strToFaceCube' s
}

strToFaceCube' :: String -> [Color]
strToFaceCube' []     = []
strToFaceCube' (s:ss) = charToColor s : strToFaceCube' ss

toCubieCube :: FaceCube -> CubieCube
