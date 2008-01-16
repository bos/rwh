{-- snippet where --}
foo = a
    where a = b
              where b = 2
{-- /snippet where --}

{-- snippet let --}
bar = let b = 2
      in let a = b
         in a
{-- /snippet let --}
