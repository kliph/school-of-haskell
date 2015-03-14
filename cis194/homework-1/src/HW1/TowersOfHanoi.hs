module HW1.TowersOfHanoi (hanoi) where

type Peg = Char
type Move = (Peg, Peg)

moveFrom :: Peg -> Peg -> Move
moveFrom a b = (a, b)

moveSeq :: Integer -> Peg -> Peg -> [Move]
moveSeq n a b
  | n == 1 = [moveFrom a b]
  | n > 1 = (moveFrom a b) : (moveSeq (n - 1) a b)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 0 = []
  | otherwise = (moveSeq (n - 1) a c) ++ (moveSeq 1 a b) ++ (moveSeq (n - 1) c b)
