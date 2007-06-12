{-- snippet read --}
main = do
        putStrLn "Please enter an integer:"
        inpStr <- getLine
        let inpInt = (read inpStr)::Integer
        putStrLn ("Twice " ++ show inpInt ++ " is " ++ show (inpInt * 2))
{-- /snippet read --}
