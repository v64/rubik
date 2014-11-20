module CoordCube (
    CoordCube (..),
    parityMove
) where

data CoordCube = CoordCube {
    ctwist  :: Int,
    eflip   :: Int,
    parity  :: Int,
    fr2br   :: Int,
    urf2dlf :: Int,
    ur2ul   :: Int,
    ub2df   :: Int,
    ur2df   :: Int
} deriving (Eq, Show)

parityMove :: [[Int]]
parityMove = [[1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1],
              [0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0]]
