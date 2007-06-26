{-- snippet all --}
main = printit 1
printit 11 = return ()
printit x = do print x
               printit (x + 1)
{-- /snippet all --}
