{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

-- Exercise 1
fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(x, y) -> (y, x+y)) (0, 1)

listToStream :: [a] -> Stream a
listToStream (x:xs) = S x (listToStream xs)
listToStream [] = error "list should not be empty"

-- Exercise 3
data Stream a = S a (Stream a)

streamToList :: Stream a -> [a]
streamToList (S x y) = x : (streamToList y)

instance Show a => Show (Stream a) where
    show s = show $ take 20 $ streamToList s

-- Exericse 4
streamRepeat :: a -> Stream a
streamRepeat n = S n $ streamRepeat n

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (S x y) = S (f x) (streamMap f y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = S x (streamFromSeed f (f x))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) (0 :: Integer)

-- This implementation is bad. Why?
--interleaveStreams :: Stream a -> Stream a -> Stream a
--interleaveStreams (S x y) (S x' y') = S x (S x' (interleaveStreams y y'))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (S x y) w = S x (interleaveStreams w y)

makeRuler :: Integer -> Stream Integer
makeRuler n = interleaveStreams (streamRepeat n) (streamMap (+1) (makeRuler n))

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)

-- Exercise 6
x :: Stream Integer
x = S 0 (S 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = S n (streamRepeat 0)
    negate (S x y) = S (- x) (negate y)
    (+) (S x y) (S x' y') = S (x + x') (y + y')
    (*) (S x y) b@(S x' y') = S (x * x') ((streamMap (* x) y') + (y * b))

instance Fractional (Stream Integer) where
    (/) (S x y) (S x' y') = q where
        constant = x `div` x'
        divX' = (\z -> z `div` x')
        xCoeff = streamMap divX' $ y - q * y'
        q = S constant xCoeff

fibs3 :: Stream Integer
fibs3 = x / (1 - x - (x * x))

-- [ a, b ]
-- [ c, d ]
type Matrix = (Integer, Integer, Integer, Integer)

instance Num Matrix where
    (a, b, c, d) * (a', b', c', d') = (a*a' + b*c', a*b' + b*d', c*a' + d*c', c*b' + d*d')
    (a, b, c, d) + (a', b', c', d') = (a+a', b+b', c+c', d+d')
    negate (a, b, c, d) = (-a, -b, -c, -d)

f :: Matrix
f = (1, 1, 1, 0)

fib4 :: Integer -> Integer
fib4 0 = 1
fib4 n = (\(_, _, c, _) -> c) $ f ^ n