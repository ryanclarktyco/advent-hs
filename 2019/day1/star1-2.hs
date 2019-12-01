import System.Environment

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  let total = calculateFuelForMasses (map (read::String->Int) (lines contents))
  print total

calculateFuelForMasses :: [Int] -> Int
calculateFuelForMasses l = foldr (+) 0 (map calculateFuelForMass l)

calculateFuelForMass :: Int -> Int
calculateFuelForMass x = if x >= 6
                            then (x `div` 3) - 2 + calculateFuelForMass ((x `div` 3) - 2)
                            else 0
