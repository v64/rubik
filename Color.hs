module Color (
    Color (..),
    charToColor
) where

data Color = U
           | R
           | F
           | D
           | L
           | B
    deriving (Bounded, Enum, Eq, Show)

charToColor :: Char -> Color
charToColor 'U' = U
charToColor 'R' = R
charToColor 'F' = F
charToColor 'D' = D
charToColor 'L' = L
charToColor 'B' = B
