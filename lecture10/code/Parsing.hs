module Parsing where


-- S -> D+S | D 
-- D -> 0 | 1


data S = Plus D S | Digit D
    deriving Show

data D = Zero | One 
    deriving Show

parserS :: Parser S
parserS = 
        (Plus <$> (parserD <* token '+') <*> parserS)
    <|> (Digit <$> parserD)

parserD :: Parser D
parserD = 
        (Zero <$ token '0')
    <|> (One  <$ token '1')
    

printS :: S -> String
printS (Plus d s) = printD d ++ "+" ++ printS s
printS (Digit d)  = printD d


printD :: D -> String
printD Zero = "0"
printD One  = "1"

evalS :: S -> Int
evalS (Plus d s) = evalD d + evalS s
evalS (Digit d)  = evalD d

evalD :: D -> Int
evalD Zero = 0 
evalD One  = 1

printD :: D -> String
printD 

ex1 :: S
ex1 = Plus
    One
    (Plus
        Zero
        Digit One)    


binary :: Parser Bool Natural
binary = 
    0 <$ token False        
    <|> (_ <$> (token True *> many (satisfy $ const True)))
  where
    f :: [Bool] -> Natural
    f = foldl' (\acc b -> 2 * acc + if b then 1 else 0) 1


data Person = Person
        { name      :: String
        , cellPhone :: Maybe String
        , landLine  :: Maybe String
        } deriving Show

personPhone :: Person -> Maybe String
personPhone p = cellPhone p <|> landLine p
