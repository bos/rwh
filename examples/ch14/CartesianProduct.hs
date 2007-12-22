{-- snippet comprehensive --}
comprehensive xs ys = [(x,y) | x <- xs, y <- ys]
{-- /snippet comprehensive --}
    
{-- snippet monadic --}
monadic xs ys = do { x <- xs; y <- ys; return (x,y) }
{-- /snippet monadic --}

{-- snippet blockyDo --}
blockyDo xs ys = do
    x <- xs
    y <- ys
    return (x, y)
{-- /snippet blockyDo --}

{-- snippet blockyPlain --}
blockyPlain xs ys =
    xs >>=
      \x -> ys >>=
         \y -> return (x, y)
{-- /snippet blockyPlain --}
