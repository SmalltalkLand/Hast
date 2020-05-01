module Smalltalk.Y(Mu(Mu) stY) where
    newtype Mu a = Mu (Mu a -> a)
    stY f = (\h -> h $ Mu h) (\x -> f . (\(Mu g) -> g) x $ x)