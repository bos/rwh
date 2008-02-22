module PutJSON where

import SimpleJSON

putJValue (JString s) = putStr (show s)
putJValue (JNumber n) = putStr (show n)
putJValue (JBool True) = putStr "true"
putJValue (JBool False) = putStr "false"
putJValue JNull = putStr "null"

putJValue (JObject xs) = do
    putChar '{'
    case xs of
      [] -> return ()
      (p:ps) -> do putPair p
                   putPairs ps
    putChar '}'
  where putPair (k,v)   = do putStr (show k)
                             putStr ": "
                             putJValue v
        putPairs (p:ps) = do putStr ", "
                             putPair p
                             putPairs ps
        putPairs []     = return ()

putJValue (JArray xs) = do
    putChar '['
    case xs of
      [] -> return ()
      (p:ps) -> do putJValue p
                   putJValues ps
    putChar ']'
  where putJValues (p:ps) = do putStr ", "
                               putJValue p
                               putJValues ps
        putJValues _      = return ()
