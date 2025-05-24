module Party where

import Employee
import Data.Tree
import Prelude

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

main :: IO ()
main = do
    fileContents <- readFile "./company.txt"

    let companyTree = read fileContents :: Tree Employee

    let resultGL = maxFun companyTree

    let GL employees funScore = resultGL

    putStrLn $ "Fun Score: " ++ show funScore
