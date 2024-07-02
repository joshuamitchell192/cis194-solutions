
-- Enumeration Types
data CarBrand = Tesla
           | Honda
           | Mazda
           | Kia
    deriving Show

carsList :: [CarBrand]
carsList = [Tesla, Honda]

isElectric :: CarBrand -> Bool
isElectric Tesla = True
isElectric Honda = False

-- Algebraic data types
data Result = Error String
            | Ok Double
    deriving Show

safeDiv :: Double -> Double -> Result
safeDiv _ 0 = Error "Division by Zero"
safeDiv x y = Ok (x / y)

-- Multiple constructors
data Person = Person String Int CarBrand
    deriving Show

joshua :: Person
joshua = Person "Joshua" 26 Tesla

getAge :: Person -> Int
getAge (Person _ a _) = a


data AlgebraicDataType = Constructor1 Int
                        | Constructor2 String

-- Type and data contructors must always start with a captial letter.
-- Variables and names of functions must always start with a lowercase letter.



result = getAge joshua

main :: IO ()
main = print result


