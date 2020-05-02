module Smalltalk.Hash(startup Plugin(Plugin) cat) where
    import Pipes
    import qualified Pipes.Prelude as P
    import qualified Data.Map as M
    import Data.List.Split
    import Smalltalk.Cont
    import Data.IORef
    import qualified Pipes.Lift as LiftP
    import Control.Exception
    import Control.Concurrent.Thread.Delay
    mapP :: Monad m => (a -> b) -> Pipe a b m r
    mapP f = for cat $ \x -> yield (f x)
    data Plugin m a = Plugin {inStream::String -> m a,outStream::a -> m String,lookupHijack::String -> m (Maybe (M.Map String ([String] ->  Pipe String String m ()) -> [String] -> Pipe String String m ()))}
    cat :: Monad m => Pipe a a m r
    cat = forever $ do
        x <- await
        yield x
    catAsync :: Monad m => Pipe (m a) a m r
    catAsync = forever $ do
        x <- await
        y <- x
        yield y
    startup m plugins (Nothing) a b = startup m plugins (do
yield ""
        ) a b
    startup m plugins (Just init) (Just input) (Nothing) = startup m plugins (Just init) (Just input) (Just P.stdoutLn)
    startup m plugins (Just init) (Nothing) (Just output) = startup m plugins (Just init) (Just P.stdinLn) (Just output)
    startup m plugins (Just init) (Nothing) (Nothing) = startup m plugins (Just init) (Nothing) (Just P.stdoutLn)
    startup m plugins (Just init) (Just input) (Just output) = do
        ref <- newIORef 0
        exref <- newIORef False
        let pluginRender = /s as pa v0 -> v0 >>= (/v -> let v2 = (s (fst pa) v) in if (snd pa == 0) then v2 else v2 >>= (as (plugins !! (snd pa - 1))))
        let s = /initb -> do
            (/v -> v `LiftP.catchError` (/str -> do
                caught
                )) $ forever $ do
                cmd <- await
                let l = splitOn "|" cmd
                ml1 <- foldl (/v acc -> acc >>= (/a -> v >>= (/v0 -> v0:a))) (map (/v -> let (cmd,args) = splitOn " " v in (foldl (/v0 acc -> vo >>= (/v -> if acc == Nothing then v else acc)) (map (/f -> f cmd) (map lookupHijack plugins)) (return . Just . M.lookup cmd)) >>= (/f -> f m arg) l) (return [])
                pl1 <- (mapP (foldl (.) (map (pluginRender inStream outStream) (zip plugins [0..]))) (catAsync))
                pl2 <- (mapP (foldr (.) (map (pluginRender outStream inStream) (zip plugins [0..]))) (catAsync))
                let r = (if initb then init else input) >->  (foldl (/v acc -> acc >-> (pl1) >-> v >-> (pl2)) cat ml1) >-> output
                runEffect r
        av <- Cont (/f -> map (/io -> Cont(/f2 -> io >>= f2)) ([(runEffect (init >-> s True)) >>= f,(runEffect (input >-> s False)) >>= f]))
        modifyIORef ref (+1)
        v <- readIORef ref
        let ef = if v /= 2 then forever else id
        rv <- (/m -> catch m (/e -> return Just (show (e::SomeException)))) $ ef $ do
            r <- readIORef exref
            if r then error "Restarting..." else Nothing
        if rv == Nothing then do
            delay 2000
            yield "Quit? (Y/N)" >-> output;
            v2 <- next input
            let (Right v3) = v2
            if (fst v3) == 'Y' then do
                writeIoRef exref True
                Nothing
            else startup m plugins (Just init) (Just input) (Just output)
        else Nothing