import Sized
import Scrabble
import Buffer
import qualified Buffer as Int

data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a ) (JoinList m a)

    deriving (Eq, Show)


-- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag Empty        = mempty
tag (Single m a) = m
tag (Append m x y) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

-- Exercise 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i jl =
    if i >= 0 then
        case jl of
            Single _ a              -> Just a
            Append _ leftJl rightJl
                | i < leftSize -> indexJ i leftJl
                | otherwise -> indexJ (i - leftSize) rightJl
                where leftSize = getSize (size (tag leftJl))
    else
         Nothing

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl =
    if n > 0 then
        case jl of
            Empty        -> Empty
            Single _ _   -> Empty
            Append _ leftJl rightJl
                | n == leftSize -> rightJl
                | n > leftSize  -> dropJ (n - leftSize) rightJl
                | otherwise     -> dropJ n leftJl
                where leftSize = getSize (size (tag leftJl))
    else
        jl

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a 
takeJ n jl =
    if n >= 0 then
        case jl of
            Empty       -> Empty
            Single m a  -> Single m a
            Append _ leftJl rightJl
                | n == leftSize -> rightJl
                | n > leftSize  -> takeJ (n - leftSize) rightJl
                | otherwise     -> takeJ n leftJl
                where leftSize = getSize (size (tag leftJl))
    else jl

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine input =
    Single (scoreString input) input

-- Exercise 4

instance Buffer (JoinList (Score, Size) String) where
    toString jl =
        case jl of
            Empty      -> ""
            Single (Score score, Size size) a ->
                a ++ show score ++ show size
            Append (Score score, Size size) leftJl rightJl ->
                show score ++ show size ++ toString leftJl ++ toString rightJl
    fromString input =
        case input of
            [] -> Empty
            x  -> Single (scoreString input, Size 1) input
    
    line = indexJ

    -- replaceLine = 