-- Functional composition
foo :: (b -> c) -> (a -> b) -> (a -> c)
-- foo f g = \x -> f (g x)
foo f g = f . g

comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)

foobar :: [Integer] -> Integer
foobar = sum . map (\x -> 7 * x + 2) . filter (>3)




