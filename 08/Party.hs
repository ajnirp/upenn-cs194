{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.List
import Data.Monoid
import Data.Tree
import Employee
import Text.Printf

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp { empFun = f }) (GL es totalf) = GL (e:es) (f + totalf)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL [] _) g = g
    mappend (GL (e:es) f) g = mappend (GL es f) $ glCons e g

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
printGL :: GuestList -> IO ()
printGL g = printGLFun g >> printGLEmp g
    where printGLFun :: GuestList -> IO ()
          printGLFun (GL _ f) = putStr "Total fun: " >> printf "%d\n" f
          printGLEmp :: GuestList -> IO ()
          printGLEmp (GL [] _) = putStr ""
          printGLEmp (GL (e:es) f) = (putStrLn . empName) e >> printGLEmp (GL es f)

instance Ord Employee where
    compare e1 e2 = compare (empName e1) (empName e2)

main :: IO ()
main = readFile "company.txt" >>= (printGL . sortEmps . maxFun . readEmp)
    where readEmp :: String -> Tree Employee
          readEmp s = read s :: Tree Employee
          sortEmps :: GuestList -> GuestList
          sortEmps (GL es f) = GL (sort es) f