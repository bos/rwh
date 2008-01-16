{-- snippet Coord --}
data Coord = Coord Int Int
{-- /snippet Coord --}

{-- snippet accessors --}
coordX (Coord x y) = x
coordY (Coord x y) = y
{-- /snippet accessors --}

{-- snippet niceAccessors --}
nicerX (Coord x _) = x
nicerY (Coord _ y) = y
{-- /snippet niceAccessors --}

{-- snippet Roygbiv --}

data Roygbiv = Red
             | Orange
             | Green
             | Blue
             | Indigo
             | Violet
               deriving (Show)

{-- /snippet Roygbiv --}
