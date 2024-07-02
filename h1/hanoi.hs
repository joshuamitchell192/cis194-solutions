
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n src goal tmp
    | n <= 0    = []
    | n == 1    = [(src, goal)]
    | otherwise = (hanoi (n-1) src tmp goal) ++ hanoi 1 src goal tmp ++ hanoi (n-1) tmp goal src


hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n src goal tmp1 tmp2
    | n <= 0    = []
    | n == 1    = [(src, goal)]
    | n == 3    = [(src, tmp1), (src, tmp2), (src, goal), (tmp2, goal), (tmp1, goal)]
    | otherwise = (hanoi4 (n-1) src tmp1 goal tmp2) ++ hanoi4 1 src goal tmp1 tmp2 ++ hanoi4 (n-1) tmp1 goal src tmp2

-- result = hanoi 3 "a" "b" "c"
result = hanoi4 15 "a" "b" "c" "d"


listLength :: [a] -> Int -- function type
listLength [] = 0 -- base case
listLength (x:xs) = 1 + listLength xs -- recursion case


main :: IO ()
main = print (listLength result)

