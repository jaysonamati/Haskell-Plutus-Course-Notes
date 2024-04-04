module Comonads where
import Control.Monad (forM_)

class Functor m => Comonad m where
    extract :: m a -> a -- "Co-return"
    duplicate :: m a -> m (m a) -- "co-join"

    extend :: (m a -> b) -> m a -> m b
    extend f = fmap f . duplicate


data Tape a = Tape [a] a [a] -- ... 17 5 4 3 | 42 | 7 9 121 11 ...
    deriving (Functor)

left :: Tape a -> Tape a
left (Tape (l:ls) x rs) = Tape ls l (x:rs)
left (Tape _ _ [])      = error ""

right :: Tape a -> Tape a
right (Tape ls x (r:rs)) = Tape (x:ls) r rs
right (Tape _ _ [])      = error ""

instance Comonad Tape where
    extract :: Tape a -> a
    extract (Tape _ x _) = x
    
    duplicate :: Tape a -> Tape (Tape a)
    duplicate t = Tape (tail $ iterate left t) t (tail $ iterate right t)

cellular :: (Bool -> Bool -> Bool -> Bool) -> Tape Bool -> Tape Bool
cellular f = extend g
    where 
        g :: Tape Bool -> Bool
        g (Tape (l:_) x (r:_)) = f l x r
        g _                    = error "expected infinite list"

rule18 :: Bool -> Bool -> Bool -> Bool 
rule18 l c r = (not l && not c && r) || (l && not c && not r)

initTape :: Tape Bool
initTape = Tape (repeat False) True (repeat False)

printTape :: Tape Bool -> IO ()
printTape (Tape ls x rs) = do
    forM_ (reverse (take 50 ls) ++ [x] ++ take 50 rs) $ \b ->
        putChar (if b then 'X' else '.')
    putChar '\n'

data Free f a =
      Pure a
    | Free (f (Free f a))
    deriving (Functor)

data Cofree f a = a :< f (Cofree f a)
    deriving (Functor)


instance Functor f => Comonad (Cofree f) where
    extract (a :< _) = a

    duplicate x@(_ :< as) = x :< fmap duplicate as

-- Cofree Maybe a = a :< Maybe (Cofree Maybe a) -- non-empty list
-- Cofree []    a = a :< [Cofree [] a]


data Player = X | O
    deriving (Show, Eq, Ord)

other :: Player -> Player
other X = O
other O = X

data Position = Position
    { board  :: [[Maybe Player]]
    , toMove :: Player
    } deriving (Show, Eq, Ord)


initialPosition :: Position
initialPosition = Position
    { board  = replicate
    , toMove = X
    }

hasWon :: Player -> Board -> Bool
hasWon p rows = 
       any (all (== Just p)) rows
    || any (all (== Just p)) (transpose rows)
    || all (== Just p) (diagonal rows)
    || all (== Just p) (diagonal (map reverse rows))
  where
    diagonal xs = zipWith (!!) xs [0..]

moves' :: Player -> Board -> [Board]
moves' p rows = do
    (i, row) <- zip [0..] rows
    (j, Nothing) <- zip [0..] row
    pure $ replace i (replace j (Just p)) rows

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace 0 x (_:xs) = x:xs
replace n x (y:xs) = y : replace (n - 1) x xs

moves :: Position -> [Position]
moves p 
    | hasWon X (board p) = []
    | hasWon O (board p) = []
    | otherwise          = [Position b (other $ toMove p) | b <- moves' (toMove p) (board p)]

data Rose a = Rose a [Rose a]
    deriving (Show, Functor)

instance Comonad a => Comonad (Rose a) where


ticTacToe :: Rose Position
ticTacToe = ticTacToe initialPosition

ticTacToe' :: Position -> Rose Position
ticTacToe' = Rose p (map ticTacToe' $ moves p)

data Outcome = XWins | OWins | Draw
    deriving (Show, Eq, Ord)

outcome :: Position -> Outcome
outcome pos
    | hasWon X (board pos) = XWins
    | hasWon O (board pos) = OWins
    | (toMove pos == X) && (any (\p -> outcome p == XWins) $ moves pos) = XWins
    | (toMove pos == X) && (any (\p -> outcome p == OWins) $ moves pos) = OWins
    | (toMove pos == X) = Draw 
    | (any (\p -> outcome p == OWins) $ moves pos)
    | all (\p -> outcome p == XWins) $ moves pos
    | otherwise

willWin :: Player -> Position -> Bool
willWin p pos 
    | hasWon p (board pos)         = True
    | hasWon (other p) (board pos) = False
    | toMove pos == p      = any (willWin p) (moves pos)
    | otherwise            = all (willWin $ other p) (moves pos)

willLose :: Player -> Position -> Bool
willLose p pos
    | hasWon (other p) (board pos) = True
    | toMove pos == p              = all (willLose $ other p) (moves pos)
    | otherwise                    = any (willWin $ other p) (moves pos)

rowMoves :: Player -> [Maybe Player] -> [[Maybe Player]]
rowMoves p [] = []
rowMoves p (Nothing:xs) = (Just p : xs) : map (Nothing:) (rowMoves p xs)
rowMoves p (Jus)