import ExprT
import Parser
import StackVM
import qualified Data.Map as M
import qualified Data.Maybe as M

-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- Exercise 1
eval :: ExprT -> Integer
eval (ExprT.Lit i) = i
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

-- Exercis 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . Parser.parseExp ExprT.Lit ExprT.Add ExprT.Mul

-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

reify :: ExprT -> ExprT
reify = id

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

-- Exercise 4
instance Expr Integer where
    lit x = x
    add x y = x + y
    mul x y = x * y

instance Expr Bool where
    lit x = x > 0
    add x y = x || y
    mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 (x + y `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 (x * y `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer

testBool = testExp :: Maybe Bool

testMM = testExp :: Maybe MinMax

testSat = testExp :: Maybe Mod7

-- Exercise 5

instance Expr Program where
    lit x = [StackVM.PushI x]
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y  ++ [StackVM.Mul]


compile :: String -> Maybe Program
compile = parseExp lit add mul

testProgram :: Maybe StackVM.Program
testProgram = testExp

-- exp :: Expr a => a

-- Exercise 6

class HasVars a where
    var :: String -> a

data VarExprT = VarLit Integer
              | VarAdd VarExprT VarExprT
              | VarMul VarExprT VarExprT
              | Var String deriving (Show, Eq)

instance Expr VarExprT where
    lit = VarLit
    add = VarAdd
    mul = VarMul

instance HasVars VarExprT where
    var = Var

type MapVar = M.Map String Integer -> Maybe Integer

instance HasVars MapVar where
    var  = M.lookup

instance Expr MapVar where
    lit f _ = Just f
    add f g m = if M.isJust (f m) && M.isJust (g m) then
                    Just (M.fromJust (f m) + M.fromJust (g m) )
                else
                    Nothing
    mul f g m = if M.isJust (f m) && M.isJust (g m) then
                    Just (M.fromJust (f m) + M.fromJust (g m))
                else
                    Nothing


withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs




