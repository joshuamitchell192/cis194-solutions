-- Parametricity

-- f must work uniformly for all types that could
-- be passed to the function.Applicative

-- Haskell does not have the ability to check types

-- f :: a -> a -> as
-- f x y = x && y

f1 :: a -> a -> a
f1 x y = x

f2 :: a -> a
f2 x = x

-- Type class listable
class Listable a where
    toList :: a -> [Int]

instance Listable Int where
    toList x = [x]

instance Listable Bool where
    toList True  = [1]
    toList False = [0]

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
    toList Empty        = []
    toList (Node x l r) = toList l ++ [x] ++ toList r

sumL x = sum (toList x)