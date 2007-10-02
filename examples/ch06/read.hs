{-- snippet read --}
main = do
        putStrLn "Please enter a Double:"
        inpStr <- getLine
        let inpInt = (read inpStr)::Double
        putStrLn ("Twice " ++ show inpInt ++ " is " ++ show (inpInt * 2))
{-- /snippet read --}
