{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List

getSubseq' :: [(Int, a)] -> Int -> [a]
getSubseq' xs n = map snd $ filter (\(m, _) -> mod m n == 0) xs

getSubseq :: [a] -> Int -> [a]
getSubseq = getSubseq' . zip [1..]

-- Exercise 1
skip :: [a] -> [[a]]
skip xs = map (\i -> getSubseq xs i) [1..(length xs)]

getFst :: (a, (b, c)) -> b
getFst (_, (x, _)) = x

-- [2,9,5,6,1] -> [(2,(9,5)),(9,(5,6)),(5,(6,1))]
groupNeighbours :: [a] -> [(a, (a, a))]
groupNeighbours x = zip x $ tail $ zip x $ tail x

-- Exercise 2
localMaxima :: (Ord a) => [a] -> [a]
localMaxima xs = map getFst $ filter (\(x, (y, z)) -> y > x && y > z) $ groupNeighbours xs

count :: Integer -> [Integer] -> Int
count n = length . filter (n ==)

counts :: [Integer] -> [Int]
counts xs = map (\i -> count i xs) [0 .. 9]

bar :: Int -> String
bar n = replicate n '*'

bars :: [Int] -> [String]
bars = map bar

padBar :: Int -> String -> String
padBar n s = replicate (n - length s) ' ' ++ s

padBars :: [String] -> [String]
padBars ss = map (padBar maxLength) ss
    where lengths = map length ss
          maxLength = maximum lengths

-- Exercise 3
histogram :: [Integer] -> String
histogram xs = graph ++ footer
    where padded = padBars $ (bars . counts) xs
          rotated = transpose padded
          graph = intercalate "\n" rotated
          footer = "\n==========\n0123456789\n"