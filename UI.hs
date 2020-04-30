module Smalltalk.UI.Base() where
    import Smalltalk.Base
    import Pipes
    import           GI.Gtk
    import           GI.Gtk.Declarative
    import           GI.Gtk.Declarative.App.Simple
    import Data.Array
    import Control.Concurrent.Thread.Delay
    process [] v = v
    process (fsts:rsts) v = fsts v >>= (process rsts)
    toIO:: Monad x -> IO x
    toIO old = do
        putStr ""
        old
    data Run t = Running t | Closed
    data SUILocation = Desktop | Settings
    data SUIState = NonManaged {internal::(Run SUILocation)} | Managed {base::SUIState,eventStream::Producer SUIEvent IO ()} 
    data SUIEvent = Hide | Close | Show | NullSUIEvent | WrapAsyncSUI {evtAsync::IO SUIEvent} 
    data CloseDialog = Waiting | CDHasEvent {evt :: SUIEvent} | InProcessOfClosing {evt::SUIEvent} 
    data CDEvent = SetToCDHasEvent | CDDefault {evt::SUIEvent} 
    data Desktop = Desktop {windows::Array Widget} 
    systemUiView (NonManaged (Closed)) = bin Window [] (widget Button [on #clicked (Show)])
    systemUiView (NonManaged (Running s)) = bin Window [on #deleteEvent (Hide)] (container Notebook [] (map (page "Desktop" . (/d -> map (bin Window []) (windows d)) []))
    systemUiView (Managed base) = systemUiView base
    closeSysUI (Waiting) = bin Window [] (container Box [] (map (BoxChild defaultBoxChildProperties) [
        widget Button [on #clicked (CDDefault Close), #label := "Close Permanently"]
        widget Button [on #clicked (CDDefault Hide), #label := "Hide"]
    ]))
    closeSysUI (InProcessOfClosing _) = bin Window [] (widget Label [#label := "Closing..."])
    closeSysUI (CDHasEvent _) = Nothing
    closeDialogReduce _ (Waiting) (CDDefault v) = Transition (InProcessOfClosing v) (return (Just (CDefault Close)))
    closeDialogReduce _ (Waiting) (SetToCDHasEvent) = Transition (Waiting) (return Nothing)
    closeDialogReduce (True) (InProcessOfClosing v) (SetToCDHasEvent) = Transition (CDHasEvent v) (return Nothing)
    closeDialogReduce (False) (InProcessOfClosing v) (SetToCDHasEvent) = Exit
    closeDialogReduce (True) (InProcessOfClosing v) (CDDefault Close) = Transition (InProcessOfClosing v) (let n = 19995 in (((toIO (Cont (/f -> [(delay n) >>= f,(/v -> f2 v
        where
            f2 (Hide) = Nothing
            f2 (Show) = Nothing
            f2 (NullSUIEvent) = Nothing
            f2 (WrapAsyncSUI x) = Nothing
            f2 (Close) = Just (f Nothing)
    ) $ (evt) $ run App{view = closeSysUI, update = closeDialogReduce False,initialState = Waiting,inputs: [p1 where
        p1 :: Producer CDEvent IO ()
        p1 = do
            delay n
            yield SetToCDHasEvent
        ]}) ]))) >> (return SetToCDHasEvent::IO CDEvent)))
    closeDialogReduce (True) (InProcessOfClosing v) _ = Transition (CDHasEvent v) (return Nothing)
    closeDialogReduce (False) (InProcessOfClosing v) _ = Transition (CDHasEvent v) (return Nothing)
    closeDialogReduce _ (CDHasEvent evt) _ = Exit
    systemUIReduce (s) (WrapAsyncSUI v) = Transition (s) (v)
    systemUiReduce (NonManaged _) (Show) = Transition (NonManaged (Run Desktop)) (return Nothing)
    systemUiReduce (NonManaged _) (Hide) = Transition (NonManaged Closed) (let c = Cont (/f -> [(evt) $ run App{view = closeSysUI, update = closeDialogReduce True,initialState = Waiting,inputs: []}) >>= f,(delay 20000) >>= (/_ -> f Hide)]) in toIO c)
    systemUiReduce (NonManaged _) (Close) = Exit
    systemUIReduce (NonManaged x) (NullSUIEvent) = Transition (NonManaged x) (return Nothing)
    systemUiReduce (Managed base stream) evt =
        let (Right (v,nextstream)) = next stream in
            let (Transition a b) = (systemUiReduce base evt) in
                Transition (Managed (a) if b == Nothing then nextdstream else stream) ((return::IO (Cont (/f -> [(if b == Nothing then return v else let (Maybe v2) = b in v2) >>= f,((delay 2000000)  >> (return (Just NullSUIEvent)::IO SUIEvent)) >>= f]))) >>= (/evt -> WrapAsyncSUI (do
                    putStr ""
                    evt
                )))