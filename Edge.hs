module Edge (
    Edge (..)
) where

data Edge = UR
          | UF
          | UL
          | UB
          | DR
          | DF
          | DL
          | DB
          | FR
          | FL
          | BL
          | BR
    deriving (Eq, Show)
