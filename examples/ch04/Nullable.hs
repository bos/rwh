{-- snippet Nullable --}
data Nullable a = Really a
                | Null
{-- /snippet Nullable --}

{-- snippet wrappedTypes --}
nullableBool = Really True

nullableString = Really "something"
{-- /snippet wrappedTypes --}
                

{-- snippet parens --}
wrapped = Really (Really "wrapped")
{-- /snippet parens --}
