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
sectionNumber (Section num title) = n
sectionTitle (Section num title) = t
{-- /snippet accessors --}

{-- snippet niceAccessors --}
nicerNumber (Section num _) = num
nicerTitle (Section _ title) = title
{-- /snippet niceAccessors --}
