import Prelude hiding (foldr, foldl)

foldr f z [] = z
foldr f z (x:xs) = x `f` foldr f z xs

foldl f z [] = z
foldl f z (x:xs) = let z' = z `f` x
                   in foldl f z' xs

foldl' f z [] = z
foldl' f z (x:xs) = let z' = z `f` x
                    in seq z' $ foldl' f z' xs


-- Strict Evaluation

-- Function arguments are completely evaluated before passing them to the function.
-- This provides predicatability of when and in what order things will happen.

-- This is a requirement when you have side-effects,
-- as the order of operations could change the outcome.

-- Lazy Evaluation

-- Evaluation is is delayed as long as possible where they're not evaulated until it becomes required.
-- When some expression is given as an argument to a function, it's packaged up as a unevaluated expression called a "thunk".

-- Pattern matching drives evaluation

-- thunks are only evaluated enough to allow a pattern match to proceed and only when pattern matched.

-- Consequences

-- Purity is rqeuired due to side effects potential to make calculations unpredictable
-- Understanding space usage (refer to foldl foldr foldl')
-- Short-circuiting operators are a special case in languages like C++ or Java where they used strict evaluation.
-- In Haskell, short circuiting can be defined without any special cases.

(&&) :: Bool -> Bool -> Bool
True && x = x
False && _ = False

-- When the first argument is False, the second argument is ignored

-- User-defined control structures
-- if has special behaviour in other languages, like short-circuiting Boolean operators.
-- Based on the value of the test, it evaluates only one of the two branches.
-- Haskell can define this as a library function.

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

-- Infinite Data Structures
-- Defining an infinite data structure actually only creates a thunk, which can be thought of a as seed
-- out of which the entire data structure grows





