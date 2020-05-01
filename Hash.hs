module Smalltalk.Hash(startup) where
    import Pipes
    import qualified Pipes.Prelude as P
    import qualified Data.Map as M
    import Data.List.Split
    import Smalltalk.Cont
    import Data.IORef
    cat :: Monad m => Pipe a a m r
    cat = forever $ do
        x <- await
        yield x
    startup m (Nothing) a b = startup (do
yield ""
        ) a b
    startup m (Just init) (Just input) (Nothing) = startup m (Just init) (Just input) (Just P.stdoutLn)
    startup m (Just init) (Nothing) (Just output) = startup m (Just init) (Just P.stdinLn) (Just output)
    startup m (Just init) (Nothing) (Nothing) = startup m (Just init) (Nothing) (Just P.stdoutLn)
    startup m (Just init) (Just input) (Just output) = do
        ref <- newIORef 0
        let s = /initb -> do
            forever $ do
                cmd <- await
                let l = splitOn "|" cmd
                let ml1 = map (/v -> let (cmd,args) = splitOn " " v in M.lookup cmd m args) l
                let r = (if initb then init else input) >->  (foldl (/v acc -> acc >-> v) cat ml1) >-> output
                runEffect r
        av <- Cont (/f -> map (/io -> Cont(/f2 -> io >>= f2)) ([(runEffect (init >-> s True)) >>= f,(runEffect (input >-> s False)) >>= f]))
        modifyIORef ref (+1)
        v <- readIORef ref
        let ef = if v /= 2 then forever else (/v -> v)
        ef $ do
            Nothing