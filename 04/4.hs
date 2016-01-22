{-# OPTIONS_GHC -Wall #-}

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z [] = z
foldl' f z (x:xs) = let z' = f z x in seq z' $ foldl' f z' xs

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

-- Exercise 1
fun1' :: [Integer] -> Integer
fun1' = foldl' (\a e -> a * (if even e then e - 2 else 1)) 1

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

-- Exercise 2
fun2' :: Integer -> Integer
fun2' = sum . (filter even) . (takeWhile (/= 1)) . (iterate next)
    where next x = if even x then x `div` 2 else 3*x+1


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

slice :: Int -> Int -> [a] -> [a]
slice from to ys = take (to - from + 1) (drop from ys)

getHeight :: Tree a -> Integer
getHeight Leaf = -1
getHeight (Node h _ _ _) = h

-- Exercise 2
foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree [x] = Node 0 Leaf x Leaf
foldTree xs = Node height (foldTree left) midElem (foldTree right)
    where len = length xs
          mid = (len - 1) `div` 2
          midElem = xs !! mid
          left = slice 0 (mid - 1) xs
          right = slice (mid + 1) (len - 1) xs
          leftRec = foldTree left
          rightRec = foldTree right
          height = 1 + max (getHeight leftRec) (getHeight rightRec)

singleXor :: Bool -> Bool -> Bool
singleXor False False = False
singleXor True False = True
singleXor False True = True
singleXor True True = False

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldl singleXor False

-- Exercise 3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\e a -> (f e) : a) []

-- Exercise 3
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) removed
    where cartProds = cartProd [1..n] [1..n]
          pairs = filter (\(i, j) -> i <= j) cartProds
          toRemove = map (\(i ,j) -> i + j + 2*i*j) cartProds
          removed = filter (\x -> not (elem x toRemove)) [1..n]
