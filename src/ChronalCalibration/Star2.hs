module ChronalCalibration.Star2 (main) where

import ChronalCalibration.Shared
import Data.Set
import Debug.Trace

main :: String -> Int
main input = findDuplicateFrequency (cycle (massageData input))

findDuplicateFrequency :: [Int] -> Int
findDuplicateFrequency input = findDuplicateFrequencyHelper input 0 $ singleton 0

findDuplicateFrequencyHelper :: [Int] -> Int -> Set Int -> Int
findDuplicateFrequencyHelper [] frequency set = error "Didn't find a duplicate frequency"
findDuplicateFrequencyHelper (x:xs) frequency set
  | member newFrequency set = newFrequency
  | otherwise = findDuplicateFrequencyHelper xs newFrequency (insert newFrequency set)
    where newFrequency = frequency + x
