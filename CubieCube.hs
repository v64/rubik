module CubieCube (
    CubieCube (..),
    idCubieCube,
    toCoordCube,
    edgeMultiply,
    getFr2Br,
    moveCube
) where

import CoordCube
import Corner
import Data.List (foldl')
import Edge

data CubieCube = CubieCube {
    cp :: [Corner],
    co :: [Int],
    ep :: [Edge],
    eo :: [Int]
} deriving (Eq, Show)

idCubieCube :: CubieCube
idCubieCube = CubieCube {
    cp = [URF, UFL, ULB, UBR, DFR, DLF, DBL, DRB],
    co = [0, 0, 0, 0, 0, 0, 0, 0],
    ep = [UR, UF, UL, UB, DR, DF, DL, DB, FR, FL, BL, BR],
    eo = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
}

rawCubieCube :: [Corner] -> [Int] -> [Edge] -> [Int] -> CubieCube
rawCubieCube cp' co' ep' eo' = CubieCube {
    cp = cp',
    co = co',
    ep = ep',
    eo = eo'
}

moveCube :: [CubieCube]
moveCube = [cc0,cc1,cc2,cc3,cc4,cc5]
    where cc0 = rawCubieCube cpU coU epU eoU
          cc1 = rawCubieCube cpR coR epR eoR
          cc2 = rawCubieCube cpF coF epF eoF
          cc3 = rawCubieCube cpD coD epD eoD
          cc4 = rawCubieCube cpL coL epL eoL
          cc5 = rawCubieCube cpB coB epB eoB
          cpU = [UBR, URF, UFL, ULB, DFR, DLF, DBL, DRB]
          coU = [0, 0, 0, 0, 0, 0, 0, 0]
          epU = [UB, UR, UF, UL, DR, DF, DL, DB, FR, FL, BL, BR]
          eoU = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
          cpR = [DFR, UFL, ULB, URF, DRB, DLF, DBL, UBR]
          coR = [2, 0, 0, 1, 1, 0, 0, 2]
          epR = [FR, UF, UL, UB, BR, DF, DL, DB, DR, FL, BL, UR]
          eoR = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
          cpF = [UFL, DLF, ULB, UBR, URF, DFR, DBL, DRB]
          coF = [1, 2, 0, 0, 2, 1, 0, 0]
          epF = [UR, FL, UL, UB, DR, FR, DL, DB, UF, DF, BL, BR]
          eoF = [0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0]
          cpD = [URF, UFL, ULB, UBR, DLF, DBL, DRB, DFR]
          coD = [0, 0, 0, 0, 0, 0, 0, 0]
          epD = [UR, UF, UL, UB, DF, DL, DB, DR, FR, FL, BL, BR]
          eoD = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
          cpL = [URF, ULB, DBL, UBR, DFR, UFL, DLF, DRB]
          coL = [0, 1, 2, 0, 0, 2, 1, 0]
          epL = [UR, UF, BL, UB, DR, DF, FL, DB, FR, UL, DL, BR]
          eoL = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
          cpB = [URF, UFL, UBR, DRB, DFR, DLF, ULB, DBL]
          coB = [0, 0, 1, 2, 0, 0, 2, 1]
          epB = [UR, UF, UL, BR, DR, DF, DL, BL, FR, FL, UB, DB]
          eoB = [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1]

toCoordCube :: CubieCube -> CoordCube
toCoordCube cc = CoordCube {
    ctwist  = getCtwist  cc,
    eflip   = getEflip   cc,
    parity  = getParity  cc,
    fr2br   = getFr2Br   cc,
    urf2dlf = getUrf2Dlf cc,
    ur2ul   = getUr2Ul   cc,
    ub2df   = getUb2Df   cc,
    ur2df   = getUr2Df   cc
}

getCtwist :: CubieCube -> Int
getCtwist cc = foldl' (\n x -> 3 * n + x) 0 $ init $ co cc

getEflip :: CubieCube -> Int
getEflip cc = foldl' (\n x -> 2 * n + x) 0 $ init $ eo cc

getParity :: CubieCube -> Int
getParity cc  = n `mod` 2
    where cp' = cp cc
          ps  = [ (cp' !! j, cp' !! i)
                | i <- [7,6..1],
                  j <- [i-1,i-2..0]
                ]
          n   = foldl' (\k (a,b) ->
                        if   a > b
                        then k+1
                        else k)
                0 ps

getFr2Br :: CubieCube -> Int
getFr2Br cc = 24 * a + b
    where a = getFr2BrA cc
          b = getFr2BrB cc

getFr2BrA :: CubieCube -> Int
getFr2BrA cc = getFr2BrA' 0 $ reverse $ ep cc

getFr2BrA' :: Int -> [Edge] -> Int
getFr2BrA' _ []     = 0
getFr2BrA' x (e:es) = e' + getFr2BrA' x' es
    where (e',x')   = if   FR <= e && e <= BR
                      then (ch, x+1)
                      else (0,  x)
          ch        = (11-i) `choose` (x+1)
          i         = length es

getFr2BrB :: CubieCube -> Int
getFr2BrB cc = getFr2BrB' (getFr2BrEdge4 cc) 3 0

getFr2BrB' :: [Edge] -> Int -> Int -> Int
getFr2BrB' _  0 b = b
getFr2BrB' es i b = getFr2BrB' es' i' b'
    where (es',k) = rotateFr2BrEdge4 es i
          i'      = i-1
          b'      = (i+1) * b + k

getFr2BrEdge4 :: CubieCube -> [Edge]
getFr2BrEdge4 cc = reverse $ getFr2BrEdge4' $ reverse $ ep cc

getFr2BrEdge4' :: [Edge] -> [Edge]
getFr2BrEdge4' []     = []
getFr2BrEdge4' (e:es) = e' ++ getFr2BrEdge4' es
    where e' = if FR <= e && e <= BR then [e] else []

rotateFr2BrEdge4 :: [Edge] -> Int -> ([Edge], Int)
rotateFr2BrEdge4 = rotateFr2BrEdge4' 0

rotateFr2BrEdge4' :: Int -> [Edge] -> Int -> ([Edge], Int)
rotateFr2BrEdge4' k es i = if   ep /= i+8
                           then rotateFr2BrEdge4' k' es' i
                           else (es,k)
    where ep  = fromEnum $ es !! i
          k'  = k+1
          es' = rotateLeft 0 i es

getUrf2Dlf :: CubieCube -> Int
getUrf2Dlf cc = 720 * a + b
    where  a  = getUrf2DlfA cc
           b  = getUrf2DlfB cc

getUrf2DlfA :: CubieCube -> Int
getUrf2DlfA cc = getUrf2DlfA' 0 $ cp cc

getUrf2DlfA' :: Int -> [Corner] -> Int
getUrf2DlfA' _ []     = 0
getUrf2DlfA' x (c:cs) = c' + getUrf2DlfA' x' cs
    where (c',x')     = if   c <= DLF
                        then (ch, x+1)
                        else (0,  x)
          ch          = i `choose` (x+1)
          i           = 7 - length cs

getUrf2DlfB :: CubieCube -> Int
getUrf2DlfB cc = getUrf2DlfB' (getUrf2DlfCorner6 cc) 5 0

getUrf2DlfB' :: [Corner] -> Int -> Int -> Int
getUrf2DlfB' _  0 b = b
getUrf2DlfB' cs i b = getUrf2DlfB' cs' i' b'
    where (cs',k)   = rotateUrf2DlfCorner6 cs i
          i'        = i-1
          b'        = (i+1) * b + k

getUrf2DlfCorner6 :: CubieCube -> [Corner]
getUrf2DlfCorner6 cc = getUrf2DlfCorner6' $ cp cc

getUrf2DlfCorner6' :: [Corner] -> [Corner]
getUrf2DlfCorner6' []     = []
getUrf2DlfCorner6' (c:cs) = c' ++ getUrf2DlfCorner6' cs
    where c' = if c <= DLF then [c] else []

rotateUrf2DlfCorner6 :: [Corner] -> Int -> ([Corner], Int)
rotateUrf2DlfCorner6 = rotateUrf2DlfCorner6' 0

rotateUrf2DlfCorner6' :: Int -> [Corner] -> Int -> ([Corner], Int)
rotateUrf2DlfCorner6' k cs i = if   cp /= i
                               then rotateUrf2DlfCorner6' k' cs' i
                               else (cs,k)
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
    where (e',x')   = if   e <= UL
                      then (ch, x+1)
                      else (0,  x)
          ch        = i `choose` (x+1)
          i         = 11 - length es

getUr2UlB :: CubieCube -> Int
getUr2UlB cc = getUr2UlB' (getUr2UlEdge3 cc) 2 0

getUr2UlB' :: [Edge] -> Int -> Int -> Int
getUr2UlB' _  0 b = b
getUr2UlB' es i b = getUr2UlB' es' i' b'
    where (es',k) = rotateUr2UlEdge3 es i
          i'      = i-1
          b'      = (i+1) * b + k

getUr2UlEdge3 :: CubieCube -> [Edge]
getUr2UlEdge3 cc = getUr2UlEdge3' $ ep cc

getUr2UlEdge3' :: [Edge] -> [Edge]
getUr2UlEdge3' []     = []
getUr2UlEdge3' (e:es) = e' ++ getUr2UlEdge3' es
    where e' = if e <= UL then [e] else []

rotateUr2UlEdge3 :: [Edge] -> Int -> ([Edge], Int)
rotateUr2UlEdge3 = rotateUr2UlEdge3' 0

rotateUr2UlEdge3' :: Int -> [Edge] -> Int -> ([Edge], Int)
rotateUr2UlEdge3' k es i = if   ep /= i
                           then rotateUr2UlEdge3' k' es' i
                           else (es,k)
    where ep  = fromEnum $ es !! i
          k'  = k+1
          es' = rotateLeft 0 i es

getUb2Df :: CubieCube -> Int
getUb2Df cc = 6 * a + b
    where a = getUb2DfA cc
          b = getUb2DfB cc

getUb2DfA :: CubieCube -> Int
getUb2DfA cc = getUb2DfA' 0 $ ep cc

getUb2DfA' :: Int -> [Edge] -> Int
getUb2DfA' _ []     = 0
getUb2DfA' x (e:es) = e' + getUb2DfA' x' es
    where (e',x')   = if   UB <= e && e <= DF
                      then (ch, x+1)
                      else (0,  x)
          ch        = i `choose` (x+1)
          i         = 11 - length es

getUb2DfB :: CubieCube -> Int
getUb2DfB cc = getUb2DfB' (getUb2DfEdge3 cc) 2 0

getUb2DfB' :: [Edge] -> Int -> Int -> Int
getUb2DfB' _  0 b = b
getUb2DfB' es i b = getUb2DfB' es' i' b'
    where (es',k) = rotateUb2DfEdge3 es i
          i'      = i-1
          b'      = (i+1) * b + k

getUb2DfEdge3 :: CubieCube -> [Edge]
getUb2DfEdge3 cc = getUb2DfEdge3' $ ep cc

getUb2DfEdge3' :: [Edge] -> [Edge]
getUb2DfEdge3' []     = []
getUb2DfEdge3' (e:es) = e' ++ getUb2DfEdge3' es
    where e' = if UB <= e && e <= DF then [e] else []

rotateUb2DfEdge3 :: [Edge] -> Int -> ([Edge], Int)
rotateUb2DfEdge3 = rotateUb2DfEdge3' 0

rotateUb2DfEdge3' :: Int -> [Edge] -> Int -> ([Edge], Int)
rotateUb2DfEdge3' k es i = if   ep /= ubi+i
                           then rotateUb2DfEdge3' k' es' i
                           else (es,k)
    where ep  = fromEnum $ es !! i
          k'  = k+1
          es' = rotateLeft 0 i es
          ubi = fromEnum UB

getUr2Df :: CubieCube -> Int
getUr2Df cc = 720 * a + b
    where a = getUr2DfA cc
          b = getUr2DfB cc

getUr2DfA :: CubieCube -> Int
getUr2DfA cc = getUr2DfA' 0 $ ep cc

getUr2DfA' :: Int -> [Edge] -> Int
getUr2DfA' _ []     = 0
getUr2DfA' x (e:es) = e' + getUr2DfA' x' es
    where (e',x')   = if   e <= DF
                      then (ch, x+1)
                      else (0,  x)
          ch        = i `choose` (x+1)
          i         = 11 - length es

getUr2DfB :: CubieCube -> Int
getUr2DfB cc = getUr2DfB' (getUr2DfEdge6 cc) 5 0

getUr2DfB' :: [Edge] -> Int -> Int -> Int
getUr2DfB' _  0 b = b
getUr2DfB' es i b = getUr2DfB' es' i' b'
    where (es',k) = rotateUr2DfEdge6 es i
          i'      = i-1
          b'      = (i+1) * b + k

getUr2DfEdge6 :: CubieCube -> [Edge]
getUr2DfEdge6 cc = getUr2DfEdge6' $ ep cc

getUr2DfEdge6' :: [Edge] -> [Edge]
getUr2DfEdge6' []     = []
getUr2DfEdge6' (e:es) = e' ++ getUr2DfEdge6' es
    where e' = if e <= DF then [e] else []

rotateUr2DfEdge6 :: [Edge] -> Int -> ([Edge], Int)
rotateUr2DfEdge6 = rotateUr2DfEdge6' 0

rotateUr2DfEdge6' :: Int -> [Edge] -> Int -> ([Edge], Int)
rotateUr2DfEdge6' k es i = if   ep /= i
                           then rotateUr2DfEdge6' k' es' i
                           else (es,k)
    where ep  = fromEnum $ es !! i
          k'  = k+1
          es' = rotateLeft 0 i es

choose :: Int -> Int -> Int
choose n k
    | n == k    = 1
    | n == 0    = 0
    | k == 0    = 1
    | otherwise = (chooseIndex !! (n-1) !! (k-1)) +
                  (chooseIndex !! (n-1) !! k)

chooseIndex :: [[Int]]
chooseIndex = [ [ choose n k
                | k <- [0..]
                ]
              | n <- [0..]
              ]

slice :: Int -> Int -> [a] -> [a]
slice a b xs = take (b-a+1) . drop a $ xs

takeR :: Int -> [a] -> [a]
takeR n l = go (drop n l) l
    where go []     r      = r
          go (_:xs) (_:ys) = go xs ys

rotate :: ([a] -> [a]) -> Int -> Int -> [a] -> [a]
rotate f a b xs = take a xs ++
                  f (slice a b xs) ++
                  takeR (length xs - b - 1) xs

rotateLeft :: Int -> Int -> [a] -> [a]
rotateLeft = rotate (\xs -> tail xs ++ [head xs])

rotateRight :: Int -> Int -> [a] -> [a]
rotateRight = rotate (\xs -> last xs : init xs)

edgeMultiply :: CubieCube -> CubieCube -> CubieCube
edgeMultiply a b = a {
    ep = ePerm a b,
    eo = eOri  a b
}

ePerm :: CubieCube -> CubieCube -> [Edge]
ePerm a b = [ epa !! fromEnum (epb !! ei)
            | ei <- map fromEnum [UR ..]
            ]
    where epa = ep a
          epb = ep b

eOri :: CubieCube -> CubieCube -> [Int]
eOri a b = [ ((eob !! ei) + (eoa !! fromEnum (epb !! ei)))
              `mod` 2
            | ei <- map fromEnum [UR ..]
            ]
    where epb = ep b
          eoa = eo a
          eob = eo b
