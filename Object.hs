module Smalltalk.Objects(Object(SmallInteger,FuncObject,StringObj) ClassState(HasClass,HasNoClass) ObjectContext(ExternalObjectContext) stObject data_ scripts klass send introspect ObjectMonad(ObjectMonad) obj Send) where
    import qualified L10n as LC
    import Smalltalk.Cont
    import Data.Dynamic
    import Smalltalk.Y
    import Text.JSON
    data ClassState = HasClass {klass::Object} | HasNoClass
    data ObjectContext =
        ExternalObjectContext Dynamic [(JSObject -> Dynamic) -> JSObject -> Dynamic] |
        InternalObjectContext Int Dynamic
        deriving (Show, Eq)
    show (InternalObjectContext id data) = "<Internal Object Context id: " ++ show id ++ " data: " ++ show fromDyn(data::Show) ++ " >"
    show (ExternalObjectConext) = "<External Object Context>"
    type Send = Dynamic -> [Object] -> (Object -> Dynamic) -> Dynamic
    data Object =
        NormalObj {
            data_ :: [Object]
            ,scripts :: [String -> Dynamic]
            ,klass :: ClassState
            ,send :: Send
            ,introspect :: Send}  |
        FuncObject {data_ :: (Dynamic -> Dynamic)} |
        SmallInteger {data_ :: Int} |
        StringObject {data_ :: [Char]}
        deriving (Eq)
    instance LC.L10n locale Object where
        LC.l6e lang (NormalObj data scripts send) = send "localize" [lang]
        LC.l6e _ (SmallInteger int) = LC.l6e int
        LC.l6e _ (FuncObject func) = show func
    data ObjectMonad a = ObjectMonad {obj::Object} | ContWOM {cont::Cont a}
    cont (c::Cont) = c
    cont (ObjectMonad o) = cont ((ObjectMonad o) >>= id)
    instance Monad ObjectMonad where
        return v = ObjectMonad (stObject (InternalObjectContext 0 Nothing) (stObject (InternalObjectContext 0 Nothing) "nil" [[">>=",[FuncObject (/scripts setScripts data_ setData klass setKlass self args return -> (data_ (fst args)) v)]]]) [] [])
        >>= (ObjectMonad a) f = ContWOM (Cont (/f2 -> f2 (let s = send a in s ">>=" [] (f2 . f))) >>= cont)
        >>= (ContWOM conta) f = ContWOM (conta >>= (cont . (/v -> v >>= (id)) . f))
    stObject = /ctxt -> stY (/ setKlass klass -> stY (/setData data_ -> stY(/setScripts scripts ->
        (/a b -> a (b False) (b True)) (NormalObj data scripts (if klass == "nil" then HasNoClass else HasClass klass)) (/i -> stY (/redo m args r ->
            (if (or (klass == "nil") i) then m else (data_ (fst (filter (/a ->
                (fst (fst a)) == m) (fst (fst klass))))))
            scripts (/newScripts ->
                setScripts newScripts)
            data_ (/newData ->
                setData newData scripts)
            klass (/newKlass ->
                setKlass newKlass data scripts)
            redo
            args
            stY(/rer ->
                let (tag,_) = (randomR (20000,22048) (mkStdGen 100120))  in (/v ->
                    if v == tag then v else r v)
                . (/v -> 
                    foldl (/s v -> s "return" m rer tag v) v scripts
                ))
    )))))
