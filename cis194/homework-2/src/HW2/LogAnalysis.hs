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
parse = undefined

insert :: LogMessage -> MessageTree -> MessageTree
insert = undefined

build :: [LogMessage] -> MessageTree
build = undefined

inOrder :: MessageTree -> [LogMessage]
inOrder = undefined

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined
