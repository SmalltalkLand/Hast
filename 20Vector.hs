module Smalltalk.Twenty.Vector(Vector(Vector,Return) nextV code return callback val call traverseV) where
    import Smalltalk.Objects
    import Smalltalk.Cont
    data Vector type = Vector {code::[Cont type],return::type -> Vector type} | Return {callback::type -> Vector type,val::Cont type}
    nextV (Vector a p) = fst a >>= (/v -> (v,let r = rst a in if (length r == 1) then (Return p (fst r)) else Vector r p))
    nextV (Return c v) = v >>= (/nv -> (nv,c nv))
    code (Return _ v) = [v]
    return (Return r _) = r
    call (Vector code _ ) callback = Cont (/f -> callback (Vector code f))
    traverseV vect callback = nextV vect >>= (/nv -> let (v,rest) = nv in let (a,bb) = (callback v,traverseV rest callback) in (bb >>= (/b -> a:b)))