module ElfTuringMachine.Star2 ( main ) where

import ElfTuringMachine.Shared

main :: String -> IO Int
main input = do
  let result = tryInputs (massageData input) $ outerProduct [0..99] [0..99]
  return result

tryInputs :: [Int] -> [(Int, Int)] -> Int
tryInputs l (input:inputs)
  | runComputerWithHotFix l input == 19690720 = calculateInput input
  | otherwise = tryInputs l inputs

calculateInput :: (Int, Int) -> Int
calculateInput (noun, verb) = 100 * noun + verb

hotFixInstructions :: [a] -> (a, a) -> [a]
hotFixInstructions l (noun, verb) = replaceValueAtPosition (replaceValueAtPosition l 1 noun) 2 verb

runComputerWithHotFix :: [Int] -> (Int, Int) -> Int
runComputerWithHotFix l (noun, verb) = head (runComputerAtPosition (hotFixInstructions l (noun, verb)) 0)

outerProduct :: [a] -> [a] -> [(a, a)]
outerProduct xs ys = 
  do
    x <- xs
    y <- ys
    return (x,y)
