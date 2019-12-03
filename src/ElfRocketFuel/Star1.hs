module ElfRocketFuel.Star1 (main) where

import ElfRocketFuel.Shared
import System.Environment

main :: String -> Int
main input = calculateFuelForMasses (massageData input)

calculateFuelForMasses :: [Int] -> Int
calculateFuelForMasses l = foldr (+) 0 (map calculateFuelForMass l)

calculateFuelForMass :: Int -> Int
calculateFuelForMass x = (x `div` 3) - 2

