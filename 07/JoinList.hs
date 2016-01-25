module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single x _) = x
tag (Append x _ _) = x

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
j +++ j' = Append (t <> t') j j'
    where t = tag j
          t' = tag j'

-- Exercise 2
indexJ :: Int -> JoinList Size a -> Maybe a
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ x) = Just x
indexJ i (Append x l r)
    | i < lsize = indexJ i l
    | otherwise = indexJ (i - lsize) r
    where ltag = tag l
          lsize = getSize ltag
indexJ _ _ = Nothing

dropJ :: Int -> JoinList Size a -> JoinList u