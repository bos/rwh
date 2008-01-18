{-- snippet Nullable --}
data Nullable a = Really a
                | Null
{-- /snippet Nullable --}

{-- snippet wrappedTypes --}
nullableBool = Really True

nullableString = Really String
{-- /snippet wrappedTypes --}
                

{-- snippet parens --}
Really (Really Int)
{-- /snippet parens --}
