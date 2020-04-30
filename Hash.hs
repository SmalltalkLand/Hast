module Smalltalk.Hash(startup) where
    import Pipes
    import qualified Pipes.Prelude as P
    import qualified Data.Map as M
    import Data.List.Split
    startup m = do
        forever $ do
            cmd <- getLine
            let l = splitOn "|" cmd
            let ml1 = map (/v -> M.lookup v m) l
            let r = P.stdinLn >->  (foldl (/v acc -> acc >-> v) ml1) >-> P.stdoutLn
            runEffect r