{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Monoid
import Data.Tree
import Employee

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp { empFun = f }) (GL es totalf) = GL (e:es) (f + totalf)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL [] _) g = g
    mappend (GL (e:es) _) g = mappend (GL es 0) $ glCons e g
    -- note: the total_fun value for constructing the first argument to mappend
    --       in the RHS above is not important, I just arbitrarily chose 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2) = if f1 < f2 then g1 else g2

-- Exercise 2
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold _ z (Node { subForest = [] }) = z
treeFold f z (Node { rootLabel = r, subForest = ts }) = f r $ map (treeFold f z) ts

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e@(Emp { empFun = f }) [] = (GL [e] f, mempty)
nextLevel e gs = (bestWithMe, bestWithoutMe)
    where bestWithMe = glCons e $ mconcat $ map snd gs
          bestWithoutMe = mconcat $ map fst gs

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun t = moreFun x y
    where (x, y) = treeFold nextLevel (empty, empty) t
          empty = mempty :: GuestList

-- Exercise 5
--getFun :: GuestList -> Fun
--getFun (GL _ f) = f

--readEmp :: String -> Tree Employee
--readEmp s = read s :: Tree Employee

--main :: IO ()
---- main = readFile "company.txt" >>= putStrLn
--main = readFile "company.txt" >>= (readEmp >>= (maxFun >>= (getFun >>= putStrLn)))