{-- snippet module --}
module PutJSON where

import Control.Monad (forM_)
import SimpleJSON

putJValue :: JValue -> IO ()

putJValue (JString s) = putStr (show s)
putJValue (JNumber n) = putStr (show n)
putJValue (JBool True) = putStr "true"
putJValue (JBool False) = putStr "false"
putJValue JNull = putStr "null"
{-- /snippet module --}

{-- snippet putJValue --}
putJValue (JObject xs) = do
    putChar '{'
    case xs of
      [] -> putStr ""
      (p:ps) -> do putPair p
                   forM_ ps $ \q -> do putStr ", "
                                       putPair q
    putChar '}'
  where putPair (k,v)   = do putStr (show k)
                             putStr ": "
                             putJValue v
{-- /snippet putJValue --}

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
