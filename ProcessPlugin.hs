module Smalltalk.Hash.ProcessPlugin(ProcessPlugin) where
    import Smalltalk.Hash
    import System.Process
    import Control.Exception
    import Pipes
    import qualified Pipes.Prelude as P
    import Control.Concurrent.ParallelIO.Global
    lh cmd = do
        p <- catch (/_ -> Nothing) ((createProcess (proc cmd []){std_out = CreatePipe, std_in = CreatePipe}) >>= Just)
        if p == Nothing then p else do
            let (Just p2) = p
            forever $ do
                parallel [hGetLine (snd p2) >>= yield,
                do
                    v <- await
                    hPutStrLn (fst p2) await]
    ProcessPlugin = Plugin {inStream = return,outStream = return,lookupHijack = lh}