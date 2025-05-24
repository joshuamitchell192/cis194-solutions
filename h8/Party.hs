module Party where

import Employee
import Data.Tree

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
