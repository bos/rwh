import Control.Concurrent.STM
import Control.Monad
import GHC.Conc

someAction = undefined

{-- snippet someTransaction --}
stmTransaction :: STM (IO a)
stmTransaction = return someAction

doSomething :: IO a
doSomething = do
  ioAction <- atomically stmTransaction
  join ioAction
{-- /snippet someTransaction --}

launchTorpedoes = undefined
doStuff = undefined
mightRetry = undefined
{-- snippet bad --}
launchTorpedoes :: IO ()

notActuallyAtomic = do
  doStuff
  unsafeIOToSTM launchTorpedoes
  mightRetry
{-- /snippet bad --}
