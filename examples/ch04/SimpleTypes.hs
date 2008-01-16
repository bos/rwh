{-- snippet accessors --}
sectionNumber (BookSection n t) = n
sectionTitle (BookSection n t) = t
{-- /snippet accessors --}

{-- snippet niceAccessors --}
nicerNumber (BookSection n _) = n
nicerTitle (BookSection _ t) = t
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
