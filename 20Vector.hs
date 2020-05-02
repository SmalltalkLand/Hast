module Smalltalk.Twenty.Vector(Vector(Vector,Return) next code return callback val) where
    import Smalltalk.Objects
    import Smalltalk.Cont
    data Vector type = Vector {code::[Cont type],return::type -> Vector type} | Return {callback::type -> Vector type,val::Cont type}
    next (Vector a p) = fst a >>= (/v -> (v,let r = rst a in if (length r == 1) then (Return p (fst r)) else Vector r p))
    next (Return c v) = v >>= (/nv -> (nv,c nv))
    code (Return _ v) = [v]
    return (Return r _) = r