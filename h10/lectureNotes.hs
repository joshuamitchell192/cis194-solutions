import qualified Control.Applicative as time
type Name = String

data Employee = Employee { name    :: Name
                         , phone   :: String }
                deriving Show

-- Employee Constructor
-- Employee :: Name -> String -> Employee

-- What if we had Maybe String and Maybe Names?
-- (Maybe Name -> Maybe Stirng -> Maybe Employee)

-- or a list of String and Names
-- ([Name] -> [String] -> [Employee])

-- Generalisation
-- (a -> b -> c) -> (f a -> f b -> f c)

fmap2 :: Functor f => (a -> b -> c) -> (f a -> f b -> f c)
fmap2 h fa fb = undefined

-- This results in the following where f ends up applying to (b -> c) as fb

-- h         :: a -> (b -> c)
-- fmap h    :: f a -> f (b -> c)
-- fmap h fa :: f (b -> c)

-- This requires an operation that combines two functor values, but fmap only operates on one functor value at a time.Applicative

-- Applicative

class Functor f =>  Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    fmap :: (a -> b) -> f a -> f b

    liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
    liftA2 h fa fb = (h `fmap` fa) <*> fb

    liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
    liftA3 h fa fb fc = ((h <$> fa) <*> fb) <*> fc

-- Applicative Laws
f `fmap` x === pure f <*> x

-- Examples

-- Maybe

instance Applicative Maybe where
    pure              = Just
    Nothing <*> _     = Nothing
    _ <*> Nothing     = Nothing
    Just f <*> Just x = Just (f x)

m_name1, m_name2 :: Maybe Name
m_name1 = Nothing
m_name2 = Just "Brent"

m_phone1, m_phone2 :: Maybe String
m_phone1 = Nothing
m_phone2 = Just "555-1234"

ex01 = Employee <$> m_name1 <*> m_phone1
ex02 = Employee <$> m_name1 <*> m_phone2
ex03 = Employee <$> m_name2 <*> m_phone1
ex04 = Employee <$> m_name2 <*> m_phone2