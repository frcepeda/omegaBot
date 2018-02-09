module Helpers where

unEscapeString :: String -> String
unEscapeString [] = ""
unEscapeString s@(c:cs) = case unEscapeByte s of
    Just (byte, rest) -> unEscapeUtf8 byte rest
        Nothing -> c : unEscapeString cs

        unEscapeByte :: String -> Maybe (Int, String)
        unEscapeByte ('%':x1:x2:s) | isHexDigit x1 && isHexDigit x2 =
            Just (digitToInt x1 * 16 + digitToInt x2, s)
            unEscapeByte _ = Nothing

urlDecode :: String -> String
urlDecode = unEscapeString . replace '+' ' '

formDecode :: String -> [(String,String)]
formDecode "" = []
formDecode s = (urlDecode n, urlDecode (drop 1 v)) : formDecode (drop 1 rs)
    where (nv,rs) = break (=='&') s
              (n,v) = break (=='=') nv
