module Golf where


skipList :: Int -> [a] -> [a]
skipList interval = map head . takeWhile (not . null) . iterate (drop interval) . drop (interval - 1)

skips :: [a] -> [[a]]
skips []        = []
skips [x]       = [[x]]
skips inputList = map (`skipList` inputList) [1..(length inputList)]

isMaxima :: (Int, Int, Int) -> Bool
isMaxima (a, b, c)
    | a < b && b > c = True
    | otherwise = False

toTuple :: [Int] -> (Int, Int, Int)
toTuple [x, y, z] = (x, y, z)

localMaxima :: [Int] -> [Int]
localMaxima =  filter . (isMaxima . toTuple . take 3) . iterate (drop 1)