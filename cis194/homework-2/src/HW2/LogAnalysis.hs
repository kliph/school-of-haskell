module HW2.LogAnalysis
( parseMessage
, parse
, insert
, build
, inOrder
, whatWentWrong
) where

import Provided.Log

data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
           deriving Show

shoe :: Thing
shoe = Shoe

data FailableDouble = Failure
                    | OK Double
                    deriving Show

data Person = Person String Int Thing
              deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

baz :: Person -> String
baz p@(Person n _ _) = "The Name field of (" ++ show p ++ ") is " ++ n

parseMessage :: String -> LogMessage
parseMessage s = case (words s !! 0) of
  ("I") -> LogMessage Info (read (words s !! 1) :: TimeStamp) (unwords (drop 2 (words s)))
  ("W") -> LogMessage Warning (read (words s !! 1) :: TimeStamp) (unwords (drop 2 (words s)))
  ("E") -> LogMessage (Error (read (words s !! 1) :: Int)) (read ((words s) !! 2) :: TimeStamp) (unwords (drop 3 (words s)))
  _ -> Unknown s

parse :: String -> [LogMessage]
parse [] = []
parse s = (parseMessage (lines s !! 0)) : (parse (unlines (drop 1 (lines s))))

getTimeStamp :: LogMessage -> Int
getTimeStamp (LogMessage _ i _) = i

getMessageString :: LogMessage -> String
getMessageString (LogMessage _ _ s) = s


insert :: LogMessage -> MessageTree -> MessageTree
insert x Leaf
  | (take 1 (show x)) == "U" = Leaf
  | otherwise = (Node (Leaf) x (Leaf))
insert x (Node l m r)
  | (take 1 (show x)) == "U" = (Node l m r)
  | getTimeStamp(x) > getTimeStamp(m) = (Node l m (insert x r))
  | otherwise = (Node (insert x l) m r)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:rest) =
  let smaller = build [a | a <- rest, getTimeStamp(a) < getTimeStamp(x)]
      bigger = build [a | a <- rest, getTimeStamp(a) > getTimeStamp(x)]
  in Node smaller x bigger
-- build (x: rest) = (insert x (build rest))

-- Node (Node Leaf (LogMessage Info 1 "") (Node Leaf (LogMessage (Error 9000) 2 "") Leaf)) (LogMessage Warning 3 "") Leaf
-- Node (Node Leaf (LogMessage Info 1 "") Leaf) (LogMessage (Error 9000) 2 "") (Node Leaf (LogMessage Warning 3 "") Leaf)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = (inOrder l) ++ [m] ++ (inOrder r)

getErrorSeverity :: LogMessage -> Int
getErrorSeverity (LogMessage (Error i) _ _) = i
-- Arbitrarily define non-error messages as having 0 severity
getErrorSeverity (LogMessage m _ _) = 0

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs =
  let overFifty = [a | a <- (inOrder (build xs)), getErrorSeverity(a) >= 50]
  in map getMessageString overFifty
