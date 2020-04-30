module Smalltalk.Base (
    stModule
    stObject
     stFFI runSTPServer
      stMessage Object ObjectContext(ExternalObjectContext) stSolveRPN
      Cont(Cont)
      callCC runCont fork forkP) where
    import System.Process
    import GHC.IO.Handle
    import Text.JSON
    import System.Random
    import Data.Dynamic
    import Control.Exception
    import qualified L10n as LC
    import Control.Concurrent (forkFinally)
    import Control.Monad (unless, forever, void)
    import qualified Data.ByteString as S
    import Network.Socket
    import Network.Socket.ByteString.Lazy (recv, sendAll)
    import qualified Network.WebSockets as WS
    import Pipes
    stS = /l -> /v -> foldl (/f v -> f v) v l
    newtype Mu a = Mu (Mu a -> a)
    stY f = (\h -> h $ Mu h) (\x -> f . (\(Mu g) -> g) x $ x)
    newtype Cont r a = Cont { runCont :: (a -> r) -> r }
    instance Monad (Cont r) where
        return a = Cont (\k -> k a)
        m >>= f = Cont (\k -> runCont m (\result -> runCont (f result) k))
    callCC f = Cont (\k -> runCont (f (\a -> Cont (\_ -> k a))) k)
    fork = Cont (/f -> [f 0,f 1])
    forkP _ :: Producer Int Cont ()
    forkP _ = fork >>= (yield)
    takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
    takeWhileInclusive _ [] = []
    takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
        else []
    stProxy pf = stY (/re f -> /v -> re (pf (f v)))
    data StStream =
        StStream {send::([Char]->IO ()),recv::(IO String)}
    runSTPServer :: Maybe HostName -> ServiceName -> (Socket -> (Int -> Int) -> IO a) -> IO a
    runSTPServer mhost port server = withSocketsDo $ do
        addr <- resolve
        bracket (open addr) close loop
      where
        resolve = do
            let hints = defaultHints {
                    addrFlags = [AI_PASSIVE]
                , addrSocketType = Stream
                }
            head <$> getAddrInfo (Just hints) mhost (Just port)
        open addr = do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            setSocketOption sock ReuseAddr 1
            withFdSocket sock $ setCloseOnExecIfNeeded
            bind sock $ addrAddress addr
            listen sock 1024
            return sock
        loop sock = forever $ do
            (conn, _peer) <- accept sock
            void $ WS.runClientWithStream :: conn mhost "/_ws" (WS.defaultConnectionOptions) [] (/conn -> callCC (server (StStream (WS.send conn) (WS.receive conn >>= (/v -> let (WS.DataMessage _ _ _ (WS.Text _ vv )) = v in 
                vv 
            )))))
    stModule = /name f all -> let require = /n -> lst fst (filter all /x -> (fst x) == n) in 
        /v -> v ([name,(f require)]: all)
    stFFI = /ffi -> /s -> fst (filter s ffi)
    stSpawnExt = /extPath -> let process = createProcess (proc extPath ["--st"]){std_out = CreatePipe,std_in = CreatePipe} in 
        let (Just hin, Just hout, _, _) = process in 
            /initData -> putStrLn initData hin >>= 
                (/n -> let funcs = [/v d -> hGetLine hout >>= v ,/v d -> putStrLn d hin >>= v] in 
                    let jsonFuncs = map (/f -> /v d -> f (/n -> v (decode n)) (encode d)) funcs in 
                        jsonFuncs)
    data ObjectContext = 
        ExternalObjectContext Dynamic [(JSObject -> Dynamic) -> JSObject -> Dynamic] |
        InternalObjectContext Int Dynamic
        deriving (Show, Eq)
    show (InternalObjectContext id data) = "<Internal Object Context id: " ++ show id ++ " data: " ++ show fromDyn(data::Show) ++ " >"
    show (ExternalObjectConext) = "<External Object Context>"
    stOcGetFFI (ExternalObjectContext ffiD) = fromDyn (ffiD::([String,Dynamic] -> Bool)->Dynamic)
    stOcGetFFI (InternalObjectContext id data) = nil
    data Object = 
        NormalObj {
            data :: [Object]
            ,scripts :: [String -> Dynamic]
            ,send :: Dynamic -> (Object -> Dynamic) -> Dynamic}  | 
        FuncObject {data :: (Dynamic -> Dynamic)} |
        SmallInteger {data :: Int}
        deriving (Eq)
    instance LC.L10n locale Object where
        LC.l6e lang (NormalObj data scripts send) = send "localize" lang
        LC.l6e _ (SmallInteger int) = LC.l6e int
        LC.l6e _ (FuncObject func) = show func
    stObject = /ctxt -> stY (/ setKlass klass -> stY (/setData data -> stY(/setScripts scripts ->
        NormalObj data scripts stY(/redo m r -> 
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
    stMessage = /f -> /m  -> Cont (/r -> f m r) 
    stSolveRPN :: (Object) => String -> a   
    stSolveRPN = head . foldl foldingFunction [] . words  
        where   foldingFunction ((Monad x):ys) str = (x >>= (/v -> foldingFunction v:(foldingFunction ys "null") str)):ys
                foldingFunction (ys) "null" = ys 
                foldingFunction (x:y:ys) "*" = SmallInteger (data x * data y):ys  
                foldingFunction (x:y:ys) "+" = SmallInteger (data x + data y):ys  
                foldingFunction (x:y:ys) "-" = SmallInteger (data y - data x):ys
                foldingFunction (ys) "array" = (stObject (InternalObjectContext 0 0) "nil" (foldl foldingFunction [] (takeWhileInclusive (/v -> v /= "arrayend") ys)) [] ):(dropWhile (/v -> v /= "arrayend") ys) 
                foldingFunction (ys) "arrayend" = ys
                foldingFunction (x:ys) "dup" = x:x:ys
                foldingFunction (x:ys) "array:first" = fst (data x):ys
                foldingFunction (i:x:ys) "array:at" = ((data x) !! (data i)):ys
                foldingFunction (n:a:ys) "array:add_all" = send a (/scripts setScripts data setData klass setKlass redo r -> setData (map (/i -> SmallInteger (data i + data n)) data))
                foldingFunction (n:a:ys) "array:mult_all" = send a (/scripts setScripts data setData klass setKlass redo r -> setData (map (/i -> SmallInteger (data i * data n)) data)) 
                foldingFunction (n:ys) "loops:repeat" = Cont (/r -> map (r . SmallInteger) [0..data n]):ys
                foldingFunction (n:ys) "cond:if" = Cont (/r -> if data n == 1 then r n else n)
                foldingFunction xs numberString = SmallInteger (read numberString):xs