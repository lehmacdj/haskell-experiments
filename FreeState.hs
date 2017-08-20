-- an implementation of the State monad using a free monad
import Control.Monad.Free

data StateF s a = Get (s -> a)
                | Put s a

type State s = Free (StateF s)

runState :: s -> State s a -> (s, a)
runState s (Pure a) = (s, a)
runState s (Free (Get f)) = runState s (f s)
runState s (Free (Put s' a)) = runState s' a
