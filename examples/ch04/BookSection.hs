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

{-- snippet accessors --}
sectionNumber (BookSection n t) = n
sectionTitle (BookSection n t) = t
{-- /snippet accessors --}

{-- snippet niceAccessors --}
nicerNumber (BookSection n _) = n
nicerTitle (BookSection _ t) = t
{-- /snippet niceAccessors --}

