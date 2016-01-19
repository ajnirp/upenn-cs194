{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Data.List (intercalate)
import Log

dropThenJoin :: Int -> [String] -> String
dropThenJoin n ws = intercalate " " $ drop n ws

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage s = case (head ws) of
	    "I" -> LogMessage Info (readIntAt 1 ws) (dropThenJoin 2 ws)
	    "W" -> LogMessage Warning (readIntAt 1 ws) (dropThenJoin 2 ws)
	    "E" -> LogMessage (Error (readIntAt 1 ws)) (readIntAt 2 ws) (dropThenJoin 3 ws)
	    _ -> Unknown $ dropThenJoin 0 ws
    where ws = words s
          readIntAt n xs = read (xs !! n) :: Int

-- Exercise 1
parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert msg Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ time1 _) (Node left msg2@(LogMessage _ time2 _) right)
	| time1 < time2 = Node (insert msg1 left) msg2 right
	| otherwise = Node left msg2 (insert msg1 right)
insert _ (Node _ (Unknown _) _) = error "MessageTree cannot contain Unknown-type messages"

-- Exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x $ build xs

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = leftTraverse ++ [msg] ++ rightTraverse
    where leftTraverse = inOrder left
          rightTraverse = inOrder right

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error n) _ _) = n > 50
isSevereError _ = False

extractMessage :: LogMessage -> String
extractMessage (LogMessage _ _ s) = s
extractMessage (Unknown s) = s

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map extractMessage) . (filter isSevereError) . (inOrder . build)