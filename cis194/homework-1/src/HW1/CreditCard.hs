module HW1.CreditCard
( toDigits
, toDigitsRev
, doubleEveryOther
, sumDigits
, validate
) where

type CCNumber = Integer

stringToInteger :: String -> Integer
stringToInteger c = read c :: Integer

splitString :: String -> [Integer]
splitString [] = []
splitString (c:rest) = stringToInteger([c]) : splitString(rest)

toDigits :: CCNumber -> [Integer]
toDigits n
  | n <= 0 = []
  | n > 0 = splitString(show n)

toDigitsRev :: CCNumber -> [Integer]
toDigitsRev n = reverse(toDigits(n))

doubleFromLeft :: [Integer] -> [Integer]
doubleFromLeft [] = []
doubleFromLeft (x:[]) = [x]
doubleFromLeft (x:(y:zs)) = x : 2 * y : doubleFromLeft zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleFromLeft . reverse

sumFlatDigits :: [Integer] -> Integer
sumFlatDigits [] = 0
sumFlatDigits(n:[]) = n
sumFlatDigits(n:ns) = n + sumFlatDigits(ns)

flattenDigitsAndSum :: Integer -> Integer
flattenDigitsAndSum n = sumFlatDigits(toDigits(n))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits(n:ns) = flattenDigitsAndSum(n) + sumDigits(ns)

validate :: CCNumber -> Bool
validate n = sumDigits(doubleEveryOther(toDigits(n))) `mod` 10 == 0
