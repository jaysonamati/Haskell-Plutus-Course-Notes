module Parser.One where

type Parse = String -> Bool


digit :: Parser   -- this parser is no
digit [c]
    | c `elem` "0123456789" = True
digit _                     = False


eof :: Parser
eof = null