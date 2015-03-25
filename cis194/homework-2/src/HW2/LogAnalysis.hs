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

parseError :: String -> MessageType
parseError s = (Error (read (drop 1 (take 3 s)) :: Int))

parseMessageType :: String -> MessageType
parseMessageType s = case s of
  ('W':s) -> Warning
  ('E':s) -> parseError(s)
  ('I':s) -> Info
             -- parseMessageType (t:_:e:rest)
             -- _ -> Unknown (e ++ rest)
takeChar :: String -> Char
takeChar(c:rest) = c

takeUntilSpace :: String -> String
takeUntilSpace s = case s of
  (' ':s) -> []
  _ -> takeChar(s) : takeUntilSpace(drop 1 s)

parseTimeStamp :: String -> TimeStamp
parseTimeStamp s
  | (parseMessageType s) == Info = (read (takeUntilSpace (drop 2 s)) :: Int)
  | (parseMessageType s) == Warning = (read (takeUntilSpace (drop 2 s)) :: Int)
  | otherwise = (read (takeUntilSpace (drop 4 s)) :: Int)

parseMessageString :: String -> String
parseMessageString s
  | (parseMessageType s) == Info = (drop (3 + (length (show (parseTimeStamp s)))) s)
  | (parseMessageType s) == Warning = (drop (3 + (length (show (parseTimeStamp s)))) s)
  | otherwise = (drop (5 + (length (show (parseTimeStamp s)))) s)


parseMessage :: String -> LogMessage
parseMessage s = LogMessage (parseMessageType s) (parseTimeStamp s) (parseMessageString s)

parse :: String -> [LogMessage]
parse x = case x of
  [] -> [Unknown "Foobar"]
  ('E':s) -> [LogMessage (Error 2) 562 "help help"]
  ('I':s) -> [LogMessage Info 29 "la la la"]

insert :: LogMessage -> MessageTree -> MessageTree
insert = undefined

build :: [LogMessage] -> MessageTree
build = undefined

inOrder :: MessageTree -> [LogMessage]
inOrder = undefined

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined
