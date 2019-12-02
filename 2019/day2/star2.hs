import System.Environment
import Data.List.Split
import Debug.Trace

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  let answer = tryInputs (map (read::String->Int) (splitOn "," contents)) $ outerProduct [0..99] [0..99]
  print answer

data Operation = Operation Int Int Int Int

tryInputs :: [Int] -> [(Int, Int)] -> Int
tryInputs l (input:inputs)
  | runComputerWithHotFix l input == 19690720 = calculateInput input
  | otherwise = tryInputs l inputs

calculateInput :: (Int, Int) -> Int
calculateInput (noun, verb) = 100 * noun + verb

compute :: Operation -> (Int, Int)
compute (Operation 1 operand1 operand2 dest) = (operand1 + operand2, dest)
compute (Operation 2 operand1 operand2 dest) = (operand1 * operand2, dest)

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

runComputerAtPosition :: [Int] -> Int -> [Int]
runComputerAtPosition l i 
  | i >= (length l) = l
  | l !! i == 99 = l
  | otherwise = runComputerAtPosition (applyMutation l (compute (getOperationForPosition l i))) (i + 4)

applyMutation :: [a] -> (a, Int) -> [a]
applyMutation l (newValue, newPosition) = replaceValueAtPosition l newPosition newValue

getOperationForPosition :: [Int] -> Int -> Operation
getOperationForPosition l i = Operation (l !! i) (loadValueFromPosition l (i + 1)) (loadValueFromPosition l (i + 2)) (l !! (i + 3))

loadValueFromPosition :: [Int] -> Int -> Int
loadValueFromPosition l i =  l !! (l !! i)

replaceValueAtPosition :: [a] -> Int -> a -> [a]
replaceValueAtPosition (x:xs) 0 y = y : xs
replaceValueAtPosition (x:xs) i y = [x] ++ replaceValueAtPosition xs (i - 1) y
