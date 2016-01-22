{-# OPTIONS_GHC -Wall #-}
module Calc where

import ExprT
import Parser (parseExp)

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = ex + ey
    where ex = eval x
          ey = eval y
eval (Mul x y) = ex * ey
    where ex = eval x
          ey = eval y

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = case parsed of
    Just e -> Just $ eval e
    Nothing -> Nothing
    where parsed = parseExp Lit Add Mul s

class Expr a where
    mul :: a -> a -> a
    lit :: Integer -> a
    add :: a -> a -> a

reify :: ExprT -> ExprT
reify = id

-- Exercise 3
instance Expr ExprT where
    mul = Mul
    lit = Lit
    add = Add

-- Exercise 4
instance Expr Integer where
    mul = (*)
    lit = id
    add = (+)

-- Exercise 4
instance Expr Bool where
    mul = (&&)
    lit = (> 0)
    add = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

-- Exercise 4
instance Expr MinMax where
    mul (MinMax x) (MinMax y) = MinMax $ x `min` y
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax $ x `max` y

-- Exercise 4
instance Expr Mod7 where
    mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7
    lit x = Mod7 $ x `mod` 7
    add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

