module FaceCube (
    FaceCube (..),
    toFaceCube,
    toCubieCube
) where

import Color
import Corner
import CubieCube
import Edge
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
    cols :: [Color]
} deriving (Eq, Show)

toFaceCube :: String -> FaceCube
toFaceCube s = FaceCube {
    cols = map charToColor s
}

toCubieCube :: FaceCube -> CubieCube
toCubieCube fc = CubieCube {
    cp = cp',
    co = co',
    ep = ep',
    eo = eo'
}
    where (cp',co') = cpAndCo fc
          (ep',eo') = epAndEo fc

cpAndCo :: FaceCube -> ([Corner], [Int])
cpAndCo fc = unzip [ (cp,co)
                   | (cc1,cc2,co) <- [ cColors (cols fc) cs
                                     | cs <- [URF ..]
                                     ],
                     cp           <- [URF ..],
                     cc1 == cornerColor !! fromEnum cp !! 1 &&
                     cc2 == cornerColor !! fromEnum cp !! 2
                   ]

cColors :: [Color] -> Corner -> (Color, Color, Int)
cColors fs c   = (cc1,cc2,co)
    where cfi  = cornerFacelet !! fromEnum c
          co   = head [ o
                      | o <- [0..2],
                        fs !! fromEnum (cfi !! o) == U ||
                        fs !! fromEnum (cfi !! o) == D
                      ]
          cc1  = fs !! fromEnum (cfi !! ((co+1) `mod` 3))
          cc2  = fs !! fromEnum (cfi !! ((co+2) `mod` 3))

epAndEo :: FaceCube -> ([Edge], [Int])
epAndEo fc = unzip [ (ep,eo)
                   | (ep,eo) <- [ eColors (cols fc) i j
                                | i <- [UR ..],
                                  j <- [UR ..]
                                ],
                     eo /= -1
                   ]

eColors :: [Color] -> Edge -> Edge -> (Edge, Int)
eColors fs i j
    | col1 == ec1 && col2 == ec2 = (j, 0)
    | col1 == ec2 && col2 == ec1 = (j, 1)
    | otherwise                  = (j,-1)
    where i'   = fromEnum i
          j'   = fromEnum j
          col1 = fs !! fromEnum ef1
          col2 = fs !! fromEnum ef2
          ef1  = head (edgeFacelet !! i')
          ec1  = head (edgeColor   !! j')
          ef2  = edgeFacelet !! i' !! 1
          ec2  = edgeColor   !! j' !! 1
