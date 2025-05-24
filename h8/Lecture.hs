

data T1 = Integer

data T2 = Char

data T3 = Bool

data D = C T1 T2 T3

data F = E { field1 :: T1, field2 :: T2, field3 :: T3 }
    deriving (Show, Read, Eq)


main :: IO ()
main = do

    let x = E { field1 = 1, field2 = 2, field3 = True }
    -- let y = x { field1 = 2 }
    putStrLn (show $ field1 x)