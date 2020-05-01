module Smalltalk.Cont(Cont(Cont) callCC) where
    newtype Cont r a = Cont { runCont :: (a -> r) -> r }
    instance Monad (Cont r) where
        return a = Cont (\k -> k a)
        m >>= f = Cont (\k -> runCont m (\result -> runCont (f result) k))
    callCC f = Cont (\k -> runCont (f (\a -> Cont (\_ -> k a))) k)