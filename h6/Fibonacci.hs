-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
-- fibs2 :: [Integer]

-- Execise 3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

-- listToStream :: [a] -> Stream a
-- listToStream [] stream = stream
-- listToStream (x:xs) stream = (Stream ())

instance Show a => Show (Stream a) where
    show :: Show a => Stream a -> String
    show = showSubset 20 1 where
        showSubset n c (Stream value nextStream)
            | c < n     = show value ++ ", " ++ showSubset n (c+1) nextStream
            | otherwise = show value

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream value nextStream) = Stream (f value) (streamMap f nextStream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f value = Stream value (streamFromSeed f (f value))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 1

ruler :: Stream Integer
ruler = streamMap largestPower nats where
    largestPower n = largest 0 n 0 where
        largest lastPower n power
          | n < 2^power = lastPower
          | n `mod` 2^power == 0 = largest power n (power + 1)
          | otherwise = largest lastPower n (power + 1)

-- Exercise 6
