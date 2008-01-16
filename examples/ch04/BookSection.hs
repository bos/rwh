{-- snippet BookSection --}
data BookSection = Section Int String
                   deriving (Show)
{-- /snippet BookSection --}

{-- snippet fp --}
fpSection = Section 5 "Functional programming"
{-- /snippet fp --}

{-- snippet BookChapter --}
data BookChapter = BookChapter Int String [BookSection]
                   deriving (Show)
{-- /snippet BookChapter --}
