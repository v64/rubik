module CoordCube (
    CoordCube (..)
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
