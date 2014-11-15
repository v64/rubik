module FaceCube (
    strToFaceCube,
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
    f :: [Color]
} deriving (Eq, Show)

strToFaceCube :: String -> FaceCube
strToFaceCube s = FaceCube {
    f = map charToColor s
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
cpAndCo fc = unzip [(j,ori) | (ori, col1, col2) <- [cornerColors fc c | c <- [URF ..]],
                              j <- [URF .. ],
                              col1 == cornerColor !! (fromEnum j) !! 1 &&
                              col2 == cornerColor !! (fromEnum j) !! 2]

cornerColors :: FaceCube -> Corner -> (Int, Color, Color)
cornerColors fc c = (ori, col1, col2)
    where i       = fromEnum c
          fs      = f fc
          ori     = head [o | o <- [0..2],
                              fs !! (fromEnum $ cornerFacelet !! i !! o) == U ||
                              fs !! (fromEnum $ cornerFacelet !! i !! o) == D]
          col1    = fs !! (fromEnum $ cornerFacelet !! i !! ((ori+1) `mod` 3))
          col2    = fs !! (fromEnum $ cornerFacelet !! i !! ((ori+2) `mod` 3))

epAndEo :: FaceCube -> ([Edge], [Int])
epAndEo fc = unzip $ filter (\(a,b) -> b /= -1) [edgeColors fc i j | i <- [UR ..], j <- [UR ..]]

edgeColors :: FaceCube -> Edge -> Edge -> (Edge, Int)
edgeColors fc i j
    | (fs !! ef) == ec  && (fs !! ef') == ec' = (j,0)
    | (fs !! ef) == ec' && (fs !! ef') == ec  = (j,1)
    | otherwise                               = (j,-1)
    where fs  = f fc
          ef  = fromEnum $ edgeFacelet !! fromEnum i !! 0
          ec  =            edgeColor   !! fromEnum j !! 0
          ef' = fromEnum $ edgeFacelet !! fromEnum i !! 1
          ec' =            edgeColor   !! fromEnum j !! 1
