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
getFr2BrA cc = getA (\e -> FR <= e && e <= BR) 11 0 $ reverse $ ep cc

getFr2BrB :: CubieCube -> Int
getFr2BrB cc = getB (\ep i -> ep /= i+8) es 3
    where es = filter (\e -> FR <= e && e <= BR) $ ep cc

getUrf2Dlf :: CubieCube -> Int
getUrf2Dlf cc = 720 * a + b
    where  a  = getUrf2DlfA cc
           b  = getUrf2DlfB cc

getUrf2DlfA :: CubieCube -> Int
getUrf2DlfA cc = getA (<= DLF) 0 7 $ cp cc

getUrf2DlfB :: CubieCube -> Int
getUrf2DlfB cc = getB (/=) cs 5
    where   cs = filter (<= DLF) $ cp cc

getUr2Ul :: CubieCube -> Int
getUr2Ul cc = 6 * a + b
    where a = getUr2UlA cc
          b = getUr2UlB cc

getUr2UlA :: CubieCube -> Int
getUr2UlA cc = getA (<= UL) 0 11 $ ep cc

getUr2UlB :: CubieCube -> Int
getUr2UlB cc = getB (/=) es 2
    where es = filter (<= UL) $ ep cc

getUb2Df :: CubieCube -> Int
getUb2Df cc = 6 * a + b
    where a = getUb2DfA cc
          b = getUb2DfB cc

getUb2DfA :: CubieCube -> Int
getUb2DfA cc = getA (\e -> UB <= e && e <= DF) 0 11 $ ep cc

getUb2DfB :: CubieCube -> Int
getUb2DfB cc = getB (\ep i -> ep /= 3+i) es 2
    where es = filter (\e -> UB <= e && e <= DF) $ ep cc

getUr2Df :: CubieCube -> Int
getUr2Df cc = 720 * a + b
    where a = getUr2DfA cc
          b = getUr2DfB cc

getUr2DfA :: CubieCube -> Int
getUr2DfA cc = getA (<= DF) 0 11 $ ep cc

getUr2DfB :: CubieCube -> Int
getUr2DfB cc = getB (/=) es 5
    where es = filter (<= DF) $ ep cc

getA :: (a -> Bool) -> Int -> Int -> [a] -> Int
getA = getA' 0

getA' :: Int -> (a -> Bool) -> Int -> Int -> [a] -> Int
getA' _ _ _ _ []     = 0
getA' i f n k (x:xs) = x' + getA' i' f n k xs
    where (x',i')    = if   f x
                       then (ch, i+1)
                       else (0,  i)
          ch         = nc `choose` (i+1)
          nc         = if n > 0 then n-l else l
          l          = if k > 0 then k - length xs else length xs

getB :: (Enum a) => (Int -> Int -> Bool) -> [a] -> Int -> Int
getB = getB' 0

getB' :: (Enum a) => Int -> (Int -> Int -> Bool) -> [a] -> Int -> Int
getB' b _ _  0    = b
getB' b f xs i    = getB' b' f xs' i'
    where (xs',k) = rotateCubie f xs i
          i'      = i-1
          b'      = (i+1) * b + k

rotateCubie :: (Enum a) => (Int -> Int -> Bool) -> [a] -> Int -> ([a], Int)
rotateCubie = rotateCubie' 0

rotateCubie' :: (Enum a) => Int -> (Int -> Int -> Bool) -> [a] -> Int -> ([a], Int)
rotateCubie' k f xs i = if   f xp i
                        then rotateCubie' k' f xs' i
                        else (xs,k)
    where xp  = fromEnum $ xs !! i
          k'  = k+1
          xs' = rotateLeft 0 i xs

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
