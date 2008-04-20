import Control.Monad.State
import Control.Monad.Reader

{-- snippet Foo --}
type Foo = StateT Int (State String)
{-- /snippet Foo --}

{-- snippet outerPut --}
outerPut :: Int -> Foo ()
outerPut = put
{-- /snippet outerPut --}

{-- snippet innerPut --}
innerPut :: String -> Foo ()
innerPut s = lift (put s)
{-- /snippet innerPut --}

{-- snippet Bar --}
type Bar = ReaderT Bool Foo

barPut :: String -> Bar ()
barPut = lift . lift . put
{-- /snippet Bar --}
