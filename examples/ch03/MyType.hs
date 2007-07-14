{-- snippet MyType --}
data MyType = MyConstructor Int String
            deriving (Show)
{-- /snippet MyType --}

{-- snippet myValue --}
myValue = MyConstructor 31337 "Creating a value!"
{-- /snippet myValue --}
