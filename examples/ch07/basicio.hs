{-- snippet all --}
-- ch08/basicio.hs
main = do
       putStrLn "Greetings!  What is your name?"
       inpStr <- getLine
       putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
{-- /snippet all --}
