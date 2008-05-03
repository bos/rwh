{-- snippet newtype --}
data DataInt = D Int
    deriving (Eq, Ord, Show)

newtype NewtypeInt = N Int
    deriving (Eq, Ord, Show)
{-- /snippet newtype --}

newtype Q = Q DataInt

an_int :: Int
an_int = 5623756

error_int :: Int
error_int = error "crash!"
