module FaceCube (
    strToFaceCube
) where

import Color
import Corner
import CubieCube
import Facelet

cornerColor :: [[Int]]
cornerColor = map (map fromEnum)
                  [[U, R, F],
                   [U, F, L],
                   [U, L, B],
                   [U, B, R],
                   [D, F, R],
                   [D, L, F],
                   [D, B, L],
                   [D, R, B]]

cornerFacelet :: [[Int]]
cornerFacelet = map (map fromEnum)
                    [[U9, R1, F3],
                     [U7, F1, L3],
                     [U1, L1, B3],
                     [U3, B1, R3],
                     [D3, F9, R7],
                     [D1, L9, F7],
                     [D7, B9, L7],
                     [D9, R9, B7]]

edgeColor :: [[Int]]
edgeColor = map (map fromEnum)
                [[U, R],
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

edgeFacelet :: [[Int]]
edgeFacelet = map (map fromEnum)
                  [[U6, R2],
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

-- faceCubeToCubieCube :: FaceCube -> CubieCube

getCornerColors :: FaceCube -> Corner -> (Color, Color)
getCornerColors fc c = (col1, col2)
    where i    = fromEnum c
          fs   = f fc
          ori  = head [o | o <- [0..2], fs !! (cornerFacelet !! i !! o) == U || fs !! (cornerFacelet !! i !! o) == D]
          col1 = fs !! (cornerFacelet !! i !! ((ori+1) `mod` 3))
          col2 = fs !! (cornerFacelet !! i !! ((ori+2) `mod` 3))
