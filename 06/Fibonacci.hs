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