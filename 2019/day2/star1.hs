import System.Environment
import Data.List.Split
import Debug.Trace

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  let finalPosition0 = runComputer (map (read::String->Int) (splitOn "," contents))
  print finalPosition0

data Operation = Operation Int Int Int Int

compute :: Operation -> (Int, Int)
compute (Operation 1 operand1 operand2 dest) = (operand1 + operand2, dest)
compute (Operation 2 operand1 operand2 dest) = (operand1 * operand2, dest)

hotFixInstructions :: [Int] -> [Int]
hotFixInstructions l = replaceValueAtPosition (replaceValueAtPosition l 1 12) 2 2

runComputer :: [Int] -> Int
runComputer l = head (runComputerAtPosition (hotFixInstructions l) 0)

runComputerAtPosition :: [Int] -> Int -> [Int]
runComputerAtPosition l i 
  | i >= (length l) = l
  | l !! i == 99 = l
  | otherwise = runComputerAtPosition (applyMutation l (compute (getOperationForPosition l i))) (i + 4)

applyMutation :: [Int] -> (Int, Int) -> [Int]
applyMutation l (newValue, newPosition) = replaceValueAtPosition l newPosition newValue

getOperationForPosition :: [Int] -> Int -> Operation
getOperationForPosition l i = Operation (l !! i) (loadValueFromPosition l (i + 1)) (loadValueFromPosition l (i + 2)) (l !! (i + 3))

loadValueFromPosition :: [Int] -> Int -> Int
loadValueFromPosition l i =  l !! (l !! i)

replaceValueAtPosition :: [a] -> Int -> a -> [a]
replaceValueAtPosition (x:xs) 0 y = y : xs
replaceValueAtPosition (x:xs) i y = [x] ++ replaceValueAtPosition xs (i - 1) y
