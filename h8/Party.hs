module Party where

import Employee
import Data.Tree
import Prelude
import Data.List (sortBy, sort)
import Data.Ord (comparing)

-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons Emp { empName, empFun } (GL employeeList funCached) =
    let updatedEmployeeList = Emp { empName, empFun } : employeeList
        newFunScore  = funCached + empFun
    in GL updatedEmployeeList newFunScore

instance Semigroup GuestList where
    (GL guestListA funA) <> (GL guestListB funB) =
        let joinedGuestList = guestListA ++ guestListB
            summedFun = funA + funB

        in GL joinedGuestList summedFun

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL employeeListA funA) (GL employeeListB funB)
    | funA > funB = GL employeeListA funA
    | funA == funB = GL employeeListA funA
    | funA < funB = GL employeeListB funB

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

-- Exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node { rootLabel, subForest }) = f rootLabel (subForest |> map (treeFold f))

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs boss subDivGLs = 
    moreFun withBoss withoutBoss
    where
        withBoss = glCons boss (mconcat subDivGLs)
        withoutBoss = mconcat subDivGLs

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss subTreeResults =
    let 
        withoutBosses = [snd treeResultPair | treeResultPair <- subTreeResults]

        withBossAdded = glCons boss (mconcat withoutBosses)

        bestSubTreeGLs = map (uncurry moreFun) subTreeResults

        -- Why is this without?
        withoutBoss = mconcat bestSubTreeGLs

    in (withBossAdded, withoutBoss)


-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun employeeTree =
    uncurry moreFun (treeFold nextLevel employeeTree)

-- Exercise 5
main :: IO ()
main = do
    fileContents <- readFile "./company.txt"

    let companyTree :: Tree Employee = read fileContents

    let resultGL = maxFun companyTree

    let GL employees funScore = resultGL

    putStrLn $ "Fun Score: " ++ show funScore

    let sortedEmployees = sortBy (comparing sortKey) employees
          where
            -- Define a sorting key as a tuple of (firstName, fullName)
            sortKey e = (firstName e, empName e)
            -- Extract the first name (first word of empName)
            firstName e = case words (empName e) of
                              (fn:_) -> fn
                              []     -> ""
    
    mapM_ (putStrLn . empName) sortedEmployees