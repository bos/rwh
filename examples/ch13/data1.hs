{-- snippet useless --}
data Silly = Foo
{-- /snippet useless --}

{-- snippet silly2 --}
data Silly2 = Foo2
     deriving (Eq, Read, Show)

toSilly2 :: a -> Silly2
toSilly2 _ = Foo2
{-- /snippet silly2 --}

{-- snippet color --}
data Color = Red | Blue | Green
    deriving (Eq, Read, Show)
{-- /snippet color --}
