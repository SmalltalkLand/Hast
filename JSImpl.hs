module Smalltalk.JS (stModule, stObject) where
    import Smalltalk.Base
    
    import Foreign.StablePtr
    type JSString = JSRef

    toJSString :: String -> JSString
    toJSString = foldl' (\s c -> js_concat s (js_string_fromchar c)) js_string_empty

    fromJSString :: JSString -> String
    fromJSString s = [js_string_tochar s i | i <- [0 .. js_length s - 1]]

    foreign import javascript "\"\"" js_string_empty
    :: JSRef

    foreign import javascript "$1.concat($2)" js_concat
    :: JSRef -> JSRef -> JSRef

    foreign import javascript "$1.length" js_length
    :: JSRef -> Int

    foreign import javascript "String.fromCodePoint($1)" js_string_fromchar
    :: Char -> JSRef

    foreign import javascript "$1.codePointAt($2)" js_string_tochar
    :: JSRef -> Int -> Char

    foreign import javascript "__asterius_jsffi.makeHaskellCallback($1)" js_make_hs_callback
    :: StablePtr (IO ()) -> IO JSRef