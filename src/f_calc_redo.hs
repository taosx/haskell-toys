{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Calc where

import ExprT
import Parser
import qualified StackVM as VM

eval :: ExprT -> Integer
eval (Lit a)    = a
eval (Add a b)  = eval a + eval b
eval (Mul a b)  = eval a * eval b 

evalStr :: String -> Maybe Integer
-- evalStr = fmap eval . parseExp Lit Add Mul -- I don't understand it yet.
evalStr s = case parseExp Lit Add Mul s of
            Just a -> Just (eval a)
            Nothing -> Nothing

class Expr a where
    lit :: Integer -> a 
    add, mul :: a -> a -> a 

reify :: ExprT -> ExprT
reify = id

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (&&)
    mul = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)


newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit a = Mod7 (a `mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

testExp :: Expr a => Maybe a 
testExp = parseExp lit add mul "(3*-4) + 5"

instance Expr VM.Program where
    lit n   = [VM.PushI n]
    add a b = a ++ b ++ [VM.Add]
    mul a b = a ++ b ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul