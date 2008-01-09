module Random where

import Control.Monad (liftM2)
import Control.Monad.State

{-- snippet rand --}
import System.Random

rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))
{-- /snippet rand --}

{-- snippet twoBadRandoms --}
twoBadRandoms :: RandomGen g => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)
{-- /snippet twoBadRandoms --}

{-- snippet twoGoodRandoms --}
twoGoodRandoms :: RandomGen g => g -> ((Int, Int), g)
twoGoodRandoms gen = let (a, gen') = random gen
                         (b, gen'') = random gen'
                     in ((a, b), gen'')
{-- /snippet twoGoodRandoms --}

{-- snippet RandomState --}
type RandomState a = State StdGen a
{-- /snippet RandomState --}

{-- snippet getRandom --}
getRandom :: Random a => RandomState a
getRandom =
  get >>= \gen ->
  let (val, gen') = random gen in
  put gen' >>
  return val
{-- /snippet getRandom --}

{-- snippet getRandomDo --}
getRandomDo :: Random a => RandomState a
getRandomDo = do
  gen <- get
  let (val, gen') = random gen
  put gen'
  return val
{-- /snippet getRandomDo --}

getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = do
  a <- getRandom
  b <- getRandom
  return (a, b)

getTwoCleaner :: Random a => RandomState (a, a)
getTwoCleaner = liftM2 (,) getRandom getRandom
