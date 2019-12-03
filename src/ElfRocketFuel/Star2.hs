module ElfRocketFuel.Star2 (main) where

import ElfRocketFuel.Shared

main :: String -> Int
main input = calculateFuelForMasses (massageData input)

calculateFuelForMasses :: [Int] -> Int
calculateFuelForMasses = sum . map calculateFuelForMass

calculateFuelForMass :: Int -> Int
calculateFuelForMass x = if x >= 6
                            then (x `div` 3) - 2 + calculateFuelForMass ((x `div` 3) - 2)
                            else 0
