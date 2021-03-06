module Corner (
    Corner (..)
) where

data Corner = URF
            | UFL
            | ULB
            | UBR
            | DFR
            | DLF
            | DBL
            | DRB
    deriving (Bounded, Enum, Eq, Ord, Show)
