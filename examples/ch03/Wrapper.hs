{-- snippet Wrapper --}
data Wrapper a = Wrapper a
                 deriving (Show)
{-- /snippet Wrapper --}

{-- snippet wrappedTypes --}
wrappedInt :: Wrapper Int

wrappedInt = Wrapper 42

wrappedString = Wrapper "foo"
{-- /snippet wrappedTypes --}

{-- snippet parens --}
multiplyWrapped :: Wrapper (Wrapper Int)

multiplyWrapped = Wrapper (Wrapper 7)
{-- /snippet parens --}
