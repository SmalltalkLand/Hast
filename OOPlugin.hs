module Smalltalk.OOPlugin(OOPlugin) where
    import Smalltalk.Objects
    import Smalltalk.Hash
    import Data.Map as M
    ohe objTable cmd = do
        let c = M.lookup cmd objTable
        v <- if c == Nothing then return Nothing else do
            let (Just nv) = c
            let (FuncObject fo) = nv
            nv
        v
    OOPlugin objTable = Plugin{inStream = return,outStream = return,lookupHijack = ohe objTable}