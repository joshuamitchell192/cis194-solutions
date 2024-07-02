

toDigits :: Integer -> [Integer]
toDigits n
    | n > 0     = toDigits (n `div` 10) ++ [n `mod` 10]
    | otherwise = []


toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n > 0     = [n `mod` 10] ++ toDigitsRev (n `div` 10)
    | otherwise = []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:z) = x : 2 * y : doubleEveryOther z

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = x
sumDigits (x:y) = x + sumDigits y

validate :: Integer -> Bool
validate x = (x `mod` 10) == 0

validateNumber :: Integer -> Bool
validateNumber = validate . sumDigits . doubleEveryOther . toDigitsRev
result = validateNumber 481

main :: IO ()
main = print result


-- toDigitsRev :: Integer -> [Integer]
