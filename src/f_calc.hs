module Calc where

import ExprT
import Parser
import qualified StackVM as VM
-- import Scientific

-- Exercise 1
eval :: ExprT -> Integer
eval e = case e of
    Lit a -> a
    Add a b -> eval a + eval b
    Mul a b -> eval a * eval b

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
            Just a -> Just (eval a)
            Nothing -> Nothing

-- Exercise 3
class Expr a where
    mul, add :: a -> a -> a
    lit      :: Integer -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit a = if (a > 0) then True else False
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
    lit a = Mod7 (a `mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

expr1 = Mul (Add (Lit 3) (Lit (4))) (Mul (Lit 2) (Lit 3))

reify :: ExprT -> ExprT
reify = id
