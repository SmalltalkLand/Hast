module Smalltalk.Init() where
    import qualified Smalltalk.Hash as H
    import qualified Data.Map as M
    import Smalltalk.Cont
    import Smalltalk.Objects
    import Smalltalk.UI.GTK
    import Smalltalk.OOPlugin
    import Pipes
    import qualified Pipes.Prelude as P
    import Data.IORef
    import qualified          GI.Gtk as Gtk
    import qualified          GI.Gtk.Declarative as D
    import qualified          GI.Gtk.Declarative.App.Simple as DAS
    fork _ = Cont (/f -> [f 0,f 1])
    defaultCMDS = M.fromList [("cat",/_ -> cat),("fork",fork),("ui-gtk",sysUIStartupTerm)]
    main = do
        writeIORef desktops []
        s <- getLine
        v <- if s == "hash" then do
            H.startup (defaultCMDS) [OOPlugin] Nothing Nothing Nothing
        else if s == "hcmd" then do
            cmd <- getLine
            H.startup (defaultCMDS) [OOPlugin] Nothing (do
                yield cmd
                for P.stdinLn yield
            ) Nothing
        else if s == "gui-gtk" then do
            runEffect (P.stdinLn >-> sysUIStartupTerm [] >-> P.stdoutLn)
        else return Nothing
        putStr "Exit? (Y/N)"
        ex <- getLine
        if (toUpper (fst ex)) == 'N' then main else return Nothing