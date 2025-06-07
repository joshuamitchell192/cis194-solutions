-- LearnYouAHaskell - The Functor typeclass
class Functor f where
    fmap :: (a -> b) -> f a -> f b
    map :: (a -> b) -> [a] -> [b]

instance Functor [] where
    fmap = map

instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x

-- Either only applies to the second argument because Functors require a kind of * -> *.
-- If f was applied to a, then a and b would have to be the same type since the f would be applied to both, so a must stay the same.