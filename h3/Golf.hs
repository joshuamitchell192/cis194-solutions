module Golf where
import Control.Monad.RWS (MonadReader(local))


skipList :: Int -> [a] -> [a]
skipList interval = map head . takeWhile (not . null) . iterate (drop interval) . drop (interval - 1)

skips :: [a] -> [[a]]
skips []        = []
skips [x]       = [[x]]
skips inputList = map (`skipList` inputList) [1..(length inputList)]

-- A recursive function that takes the first 3 elements of an Int list and check if the middle value is the maximum.
-- Then the next recursive call is made concatenated with the result if the middle element has the greatest value,
-- otherwise the function is called with the elements shift by one position.

localMaxima :: [Int] -> [Int]
localMaxima (a:b:c:xs)
    | a < b && b > c = b : localMaxima (b:c:xs)
    | otherwise =  localMaxima (b:c:xs)
localMaxima _ = []

frequency :: Int -> [Int] -> Int
frequency value = length . filter (\x -> x == value)

returnString :: Int -> Int -> Char
returnString frequency value
    | value >= frequency = '*'
    | otherwise          = ' '

createLine :: Int -> [Int] -> String
createLine freq frequencyList = map (\x -> returnString freq x ) frequencyList

toString :: [Int] -> String
toString frequencyList = (concat . reverse) (map (\frequency -> (createLine frequency frequencyList) ++ "\n") [1..(maximum frequencyList)])


histogram :: [Int] -> String
histogram xs = (toString (map (\x -> frequency x xs ) [0..9]) ++ "==========\n0123456789\n")