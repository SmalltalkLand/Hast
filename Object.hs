module Smalltalk.Objects(Object(SmallInteger,FuncObject) ClassState(HasClass,HasNoClass) ObjectContext(ExternalObjectContext) stObject data scripts klass send) where
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
    data Object =
        NormalObj {
            data :: [Object]
            ,scripts :: [String -> Dynamic]
            ,klass :: ClassState
            ,send :: Dynamic -> (Object -> Dynamic) -> Dynamic}  |
        FuncObject {data :: (Dynamic -> Dynamic)} |
        SmallInteger {data :: Int}
        deriving (Eq)
    instance LC.L10n locale Object where
        LC.l6e lang (NormalObj data scripts send) = send "localize" lang
        LC.l6e _ (SmallInteger int) = LC.l6e int
        LC.l6e _ (FuncObject func) = show func
    stObject = /ctxt -> stY (/ setKlass klass -> stY (/setData data -> stY(/setScripts scripts ->
        NormalObj data scripts (if klass == "nil" then HasNoClass else HasClass klass) stY(/redo m r ->
            (if klass == "nil" then m else (data (fst (filter (/a ->
                (fst (fst a)) == m) (fst (fst klass))))))
            scripts (/newScripts ->
                setScripts newScripts)
            data (/newData ->
                setData newData scripts)
            klass (/newKlass ->
                setKlass newKlass data scripts)
            redo
            stY(/rer ->
                let (tag,_) = (randomR (20000,22048) (mkStdGen 100120))  in (/v ->
                    if v == tag then v else r v)
                . (/v -> 
                    foldl (/s v -> s "return" m rer tag v) v scripts
                ))
    ))))