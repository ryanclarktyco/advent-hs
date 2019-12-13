module SecureContainer.Star1 () where

import Data.List
import Data.List.Split

main :: String -> Int
main input = length $ filterList [x..y]
  where (x,y) = massageData input

filterList :: [Int] -> [Int]
filterList = sieve [filterLength, filterMonotonic, filterDoubleDigit]

sieve :: [Int -> Bool] -> [Int] -> [Int]
sieve [] xs = xs
sieve (f:fs) xs = sieve fs (fst $ partition f xs)

filterDoubleDigit :: Int -> Bool
filterDoubleDigit x = filterDoubleDigitHelper $ show x

filterDoubleDigitHelper :: String -> Bool
filterDoubleDigitHelper [x] = False
filterDoubleDigitHelper (x:xs) 
  | x == head xs = True
  | otherwise = filterDoubleDigitHelper xs

filterMonotonic :: Int -> Bool
filterMonotonic x = filterMonotonicHelper $ show x

filterMonotonicHelper :: String -> Bool
filterMonotonicHelper [x] = True
filterMonotonicHelper (x:xs)
  | x > head xs = False
  | otherwise = filterMonotonicHelper xs

filterLength :: Int -> Bool
filterLength x = (length $ show x) == 6

massageData :: String -> (Int, Int)
massageData input = (read $ head chunks, read $ chunks !! 1)
  where chunks = splitOn "-" input
