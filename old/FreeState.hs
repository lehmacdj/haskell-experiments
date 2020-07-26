{-# LANGUAGE DeriveFunctor #-}

module FreeState where
-- an implementation of the State monad using a free monad
import Control.Monad.Free

data StateF s a = Get (s -> a)
                | Put s a
                deriving (Functor)

type State s = Free (StateF s)

get :: State s s
get = liftF $ Get id

put :: s -> State s ()
put s = liftF $ Put s ()

runState :: s -> State s a -> (s, a)
runState s (Pure a) = (s, a)
runState s (Free (Get f)) = runState s (f s)
runState _ (Free (Put s' a)) = runState s' a
